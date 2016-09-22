(in-package #:wiki-lang-detect)
(named-readtables:in-readtable rutilsx-readtable)
            

(defun save-model (file &optional (model *lang-detector*))
  "Dump lang-detect MODEL to a zip FILE."
  (with-open-file (out file :direction :output :if-does-not-exist :create
                       :if-exists :overwrite)
    (format out "~%"))  ; create the file beforehand as zip lib isn't able
  (zip:with-output-to-zipfile (zip file :if-exists :overwrite)
    (zip-add-text-file zip "counts.txt"
                       (fmt "words ~A~%3gs ~A"
                            @model.words-count @model.3gs-count))
    (zip-add-text-file zip "langs.txt"
                       (w/outstr (out)
                         (format out "~{~A~^~%~}"
                                 (sort (keys @model.langs) 'string<
                                       :key 'princ-to-string))))
    (when @model.bias
      (zip-add-text-file zip "bias.csv"
                         (w/outstr (out)
                           (loop :for (lang bias)
                                 :in (sort (ht->pairs @model.bias)
                                           '> :key 'rt) :do
                             (format out "~A	~A~%" lang bias)))))
    (dotable (lang _ @model.langs)
      (zip-add-text-file zip (fmt "~A-words.csv" lang)
                         (w/outstr (out)
                           (dotable (word lang-logprobs @model.words)
                             (when-it (assoc1 lang lang-logprobs)
                               (format out "~A	~A~%" word it))))))
    (dotable (lang _ @model.langs)
      (zip-add-text-file zip (fmt "~A-3gs.csv" lang)
                         (w/outstr (out)
                           (dotable (3g lang-logprobs @model.3gs)
                             (when-it (assoc1 lang lang-logprobs)
                               (format out "~A	~A~%" 3g it)))))))
  file)

(defun load-model (file)
  "Restore lang-detect model from a zip FILE."
  (format t "Loading wiki-lang-detect model from ~A " file)
  (zip:with-zipfile (zip file :force-utf-8 t)
    (with ((rez (make-lang-detector
                 :bias (ignore-errors  ; bias data may be absent
                        (pairs->ht
                         (mapcar ^(with (((k v) (split #\Tab %)))
                                    (pair (mkeyw k)
                                          (read-from-string v)))
                                 (split #\Newline
                                        (zipped-file-data zip "bias.csv")
                                        :remove-empty-subseqs t))))
                 :langs (pairs->ht
                         (mapcar ^(pair (mkeyw %) t)
                                 (split #\Newline
                                        (zipped-file-data zip "langs.txt")
                                        :remove-empty-subseqs t)))))
           ((words-count 3gs-count)
            (mapcar ^(parse-integer (second (split #\Space %)))
                    (split #\Newline (zipped-file-data zip "counts.txt")
                           :remove-empty-subseqs t))))
      (:= @rez.words-count words-count
          @rez.3gs-count 3gs-count)
      (dotable (lang _ @rez.langs)
        (when (in# (fmt "~A-words.csv" lang) (zip:zipfile-entries zip))
          (dotable (word logprob (load-ht (zipped-file-data
                                           zip (fmt "~A-words.csv" lang))))
            (push (cons lang logprob) (? @rez.words word)))
          (dotable (3g logprob (load-ht (zipped-file-data
                                         zip (fmt "~A-3gs.csv" lang))))
            (push (cons lang logprob) (? @rez.3gs 3g))))
        (princ "."))
      (format t " done.~%")
      rez)))

(defun load-ht (text &key (test 'eql))
  (let ((rez (make-hash-table :test test)))
    (with-input-from-string (in text)
      (loop :for line := (read-line in nil) :while line :do
        (with (((k v) (split-if ^(member % '(#\Space #\Tab)) line
                                :remove-empty-subseqs t)))
          (:= (? rez k) (read-from-string v)))))
    rez))

(defun prune-model (model &optional (frac 1.0))
  "Return a copy of the MODEL without the words and 3gs in wrong script
   and leave only a fraction FRAC of words in it."
  (let ((words #h())
        (3gs #h())
        (scripts #h())
        (corr (/ 1 (* 10 frac)))
        (rez (make-lang-detector
              :bias @model.bias
              :langs @model.langs
              :words-count (floor (* @model.words-count frac))
              :3gs-count (floor (* @model.3gs-count)))))
    ;; words
    (dotable (word logprobs @model.words)
      (loop :for (lang . logprob) :in logprobs :do
        (unless (in# lang words)
          (set# lang words #h(equal)))
        (:= (? words lang word) (+ logprob corr))))
    (dotable (lang logprobs words)
      (let ((lang-script #h(equal)))
        (dotable (word logprob logprobs)
          (:+ (get# (word-script word) lang-script 0)))
        (:= (? scripts lang) (car (first (sort (ht->alist lang-script)
                                               '> :key 'cdr))))))
    (dotable (lang logprobs words)
      (with ((script (? scripts lang))
             (wrds (remove-if-not ^(string= script (word-script (car %)))
                                  (sort (ht->alist logprobs) '> :key 'cdr))))
        (loop :for (word . logprob)
              :in (subseq wrds 0 (ceiling (* frac (length wrds)))) :do
          (push (cons lang logprob) (? @rez.words word)))))
    ;; 3gs
    (dotable (3g logprobs @model.3gs)
      (loop :for (lang . logprob) :in logprobs :do
        (unless (in# lang 3gs)
          (set# lang 3gs #h(equal)))
        (:= (? 3gs lang 3g) (+ logprob corr))))
    (dotable (lang logprobs 3gs)
      (let ((script (? scripts lang)))
        (loop :for (3g . logprob)
              :in (remove-if-not ^(member script (map 'list 'unicode-script
                                                      (car %))
                                          :test 'string=)
                                 (sort (ht->alist logprobs) '> :key 'cdr)) :do
          (push (cons lang logprob) (? @rez.3gs 3g)))))
    rez))

(defun huffman-model (model)
  "Return the huffmanized version of MODEL that will have 2-level (script - key)
   words & 3gs fields instead of 1-level (key)."
  (with ((rez (make-lang-detector
               :bias @model.bias
               :langs @model.langs
               :words #h(equal)
               :words-count @model.words-count
               :3gs #h(equal)
               :3gs-count @model.3gs-count
               :huffman (huffman-dict model)))
         (words @rez.words)
         (3gs @rez.3gs))
    (dotable (word logprobs @model.words)
      (let ((script (word-script word)))
        (unless (in# script words)
          (:= (? words script) #h(equal)))
        (:= (? words script (huffman-encode word rez)) logprobs)))
    (dotable (3g logprobs @model.3gs)
      (let ((script (word-script 3g)))
        (unless (in# script 3gs)
          (:= (? 3gs script) #h(equal)))
        (:= (? 3gs script (huffman-encode 3g rez)) logprobs)))
    rez))


;;; utils

(defun zipped-file-data (zip name &key (encoding :utf-8))
  "Get the contents of a file NAME inside an open zip archive ZIP.
   If ENCODING is indicated (default - :UTF-8), decode raw data as encoded characters."
  (let ((raw (zip:zipfile-entry-contents (zip:get-zipfile-entry name zip))))
    (if encoding
        (babel:octets-to-string raw :encoding encoding)
        raw)))

(defun zip-add-text-file (zip name data)
  "Add DATA as a text file named NAME to the zip archive ZIP."
  (zip:write-zipentry zip name
                      (flex:make-in-memory-input-stream
                       (babel:string-to-octets data :encoding :utf-8))
                      :file-write-date (get-universal-time)))

(defun word-script (word)
  "Return the predominant unicode script of a WORD."
  (let ((scripts #h(equal))
        (max 0)
        argmax)
    (dovec (char word)
      (:+ (get# (unicode-script char) scripts 0)))
    (dotable (script freq scripts)
      (when (> freq max)
        (:= max freq
            argmax script)))
    argmax))
