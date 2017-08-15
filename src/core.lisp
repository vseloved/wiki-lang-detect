(in-package #:wiki-lang-detect)
(named-readtables:in-readtable rutilsx-readtable)

(declaim (inline text-lang))


;;; base classes

(defstruct (token (:print-function
                   (lambda (struct stream depth)
                     (format stream "<~A -~{ ~A~^ |~}>"
                             @struct.word
                             (take 5
                                   (mapcar ^(fmt "~A:~$"
                                                 (car %) (cdr %))
                                           @struct.langs))))))
  word langs)

(defstruct (lang-detector
            (:print-function
             (lambda (m out nested)
               (declare (ignore nested))
               (format out "#<LANG-DETECTOR langs:~A words:~A 3gs:~A~@[ huffman~]>"
                       (ht-count @m.langs) @m.words-count @m.3gs-count @m.huffman))))
  (bias *internet-lang-bias*)
  (langs #h())
  (words #h(equal))
  (words-count 0)
  (3gs #h(equal))
  (3gs-count 0)
  huffman)


;;; data extraction

(defun extract-words (str)
  "Return a list of downcased words found in STR."
  (mapcar 'string-downcase
          (remove-if ^(or (blankp %)
                          (every 'digit-char-p %)
                          (find #\_ %))
                     (flat-map ^(let (cur
                                      cur-script
                                      words)
                                  (dovec (char %)
                                    (let ((script (unicode-script char)))
                                      (when (and (not (equal cur-script script))
                                                 (or (in# script *1lang-scripts*)
                                                     (in# cur-script *1lang-scripts*)))
                                        (:= cur-script script)
                                        (when cur
                                          (push (coerce (reverse cur) 'string)
                                                words)
                                          (void cur))))
                                    (push char cur))
                                  (push (strjoin "" (reverse cur)) words)
                                  (reverse words))
                               (re:all-matches-as-strings
                                "\\S+"
                                (substitute-if
                                 #\Space 'punct-char-p
                                 (re:regex-replace-all
                                  "(https?|ftp)://[^\\s/$.?#].[^\\s]*"
                                  ;; (re:regex-replace-all
                                  ;;  "(\\w)[’'](\\w)"
                                  (remove-if ^(< 767 (char-code %) 880)
                                             str)
                                  ;; "\\1­\\2")
                                  " ")))))))
(defun extract-3gs (word)
  "Return a list of all character 3grams from the current WORD
   preceded by a space and followed by a space."
  (with ((len (length word))
         (rez (make-array len)))
    (dotimes (i len)
      (let ((cur (make-string 3)))
        (:= (char cur 0) (if (plusp i) (char word (1- i)) #\Space)
            (char cur 1) (char word i)
            (char cur 2) (if (= i (1- len)) #\Space (char word (1+ i))))
        (:= (svref rez i) cur)))
    rez))


;;; core calcs

(defun word-langs (word &key (detector *lang-detector*) (n 5) (threshold 0.0)
                        only-langs known-words-threshold)
  "Return an alist of up to N langs with their probs for WORD
   using DETECTOR with a possible cutoff probability THRESHOLD."
  (with ((script (word-script word))
         (all (or (when-it (? *1lang-scripts* script)
                    (list (cons it 1.0)))
                  (if @detector.huffman
                      (when-it (? @detector.words script)
                        (? it (huffman-encode word detector)))
                      (? @detector.words word))
                  (with ((3gs-lps (map* ^(if @detector.huffman
                                             (when-it (? @detector.3gs script)
                                               (? it (huffman-encode % detector)))
                                             (? @detector.3gs %))
                                        (extract-3gs word)))
                         (3gs-count (length 3gs-lps))
                         (langs #h())
                         (rez #h()))
                    (if only-langs
                        (dolist (lang only-langs)
                          (:= (? langs lang) t))
                        (dovec (cur-logprobs 3gs-lps)
                          (loop :for (lang . logprob) :in cur-logprobs :do
                            (:= (? langs lang) t))))
                    (dovec (cur-logprobs 3gs-lps)
                      (dotable (lang _ langs)
                        (:+ (get# lang rez 0)
                            (or (assoc1 lang cur-logprobs)
                                (log (/ 1 @detector.3gs-count)))))) ; TODO: const
                    (mapcar ^(cons (car %)
                                   (/ (cdr %) 3gs-count))
                            (ht->alist rez))))))
    (or (norm (subseq (sort (remove-if ^(< (cdr %) threshold)
                                  (mapcar ^(cons (car %) (exp (cdr %)))
                                          all))
                            '> :key 'cdr)
                      0 (when n (min n (length all)))))
        '((nil . 1.0)))))

(defun word-direct-langs (word &key (detector *lang-detector*))
  "Return list of langs for WORD if it is directly known to the DETECTOR."
  (or (when-it (? *1lang-scripts* (unicode-script (? word 0)))
        (list it))
      (if @detector.huffman
          (when-it (? @detector.words (word-script word))
            (keys (? it (huffman-encode word detector))))
          (keys (? @detector.words word)))))

(defun text-langs (text &key (detector *lang-detector*)
                          (n 5) (threshold 0.1) (beam-size 30))
  "Return a sorted list of inferred language-weight pairs for TEXT
   and a vector of similar pairs for each individual words
   using DETECTOR.

   N is the number of results to return. NIL means all. Default is 5.

   THRESHOLD is the minimum weight of the lang at which it may be
   included into the results.

   BEAM-SIZE is the number of languages to consider for each word."
  (with ((toks (map 'vector ^(make-token :word %)
                    (extract-words text)))
         (tok-langs (map 'list ^(word-langs @%.word :detector detector
                                                    :n beam-size)
                         toks))
         (langs (pairs->ht (map* ^(pair % (if @detector.bias
                                              (log (get# % @detector.bias
                                                         *min-lang-bias*))
                                              0))
                                 (remove-duplicates
                                  (flat-map ^(mapcar 'car %)
                                            tok-langs)))))
         (total 0)
         (fifth (ceiling (length toks) 5))
         (max nil)
         (argmax nil)
         (prev-lang nil)
         (prev-beg nil)
         (spans nil))
    ;; kanji hack
    (dolist (lang '(:ja :ko))
      (when (> (count-if ^(assoc1 lang %) tok-langs)
               fifth)
        (:= tok-langs (substitute-if (list (cons lang 1.0))
                                     ^(assoc1 :zh %)
                                     tok-langs))))
    ;; calc lang probs
    (loop :for tok :across toks
          :for cur-langs :in tok-langs :do
      (let ((importance (word-importance @tok.word)))
        (:= @tok.langs cur-langs)
        (dotable (lang _ langs)
          (:+ (? langs lang)
              (log (* (or (assoc1 lang cur-langs)
                          (sqrt (/ 1 @detector.words-count)))
                      importance))))))
    ;; in case of too many words and too small or too large prob -
    ;; select the single best lang
    (dotable (lang logprob langs)
      (when (or (null max)
                (> logprob max))
        (:= max logprob
            argmax lang))
      (let ((prob (exp (if (> logprob 80)
                           (coerce logprob 'double-float)
                           logprob))))
        (:= (? langs lang) prob)
        (:+ total prob)))
    (if (zerop total)
        (clrhash langs)
        (dotable (lang prob langs)
          (:/ prob total)
          (if (< prob threshold)
              (rem# lang langs)
              (:= (? langs lang) prob))))
    (when (zerop (ht-count langs))
      (:= (? langs argmax) 1.0))
    (let ((all (ht->alist langs)))
      (values (subseq (sort all '> :key 'cdr) 0 (min n (ht-count langs)))
              toks))))

(defun text-lang (text &key (detector *lang-detector*) (beam-size 30)
                       (known-words-threshold 0.3))
  "Return the top language for TEXT according to the DETECTOR
   (with a specified BEAM-SIZE.)
   If KNOWN-WORDS-THRESHOLD is supplied at least a given ratio of words
   should be recognized by the detector to output non-nil lang."
  (and-it (car (first (text-langs text :detector detector :beam-size beam-size)))
          (and (or (null known-words-threshold)
                   (let ((words (extract-words text)))
                     (>= (count-if 'just (mapcar 'word-direct-langs words)) 
                         (* known-words-threshold (length words)))))
               it)))


;;; utils

(defun word-importance (word)
  "A measure of WORD importance - the shorter the better."
  (if (every 'digit-char-p word)
      1e-20
      (/ 1 (log (1+ (length word))))))

(defun norm (list)
  "Normalize all values in LIST to sum to 1."
  (let ((total (reduce '+ list :key 'cdr)))
    (mapcar ^(cons (car %) (/ (cdr %) total))
            list)))

(defun logprob (lang list &optional detector default)
  "Return a logrpob for LANG from LIST"
  (loop :for (lng . logprob) :in list :do
    (when (and (eql lang lng) logprob)
      (return-from logprob logprob)))
  (case default
    (:3g (/ 1 @detector.3gs-count)))
    (:word (/ 1 @detector.words-count)))

(let ((scripts #h()))
  (defun unicode-script (char)
    (getset# char scripts (cl-unicode:script char)))
  (defun cached-char-scripts ()
    scripts))

(let ((puncts #h()))
  (defun punct-char-p (char)
    (getset# char puncts
             (eql 'cl-unicode-names::po
                  (2nd (cl-unicode:general-category char)))))
  (defun cached-char-puncts ()
    puncts))
  
