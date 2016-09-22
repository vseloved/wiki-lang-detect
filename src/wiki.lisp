(in-package #:wiki-lang-detect)
(named-readtables:in-readtable rutilsx-readtable)


(defun download-latest-wiki (lang file &optional (overwrite t))
  "Download latest Wikipedia dump for language LANG to FILE."
  (when (or overwrite
            (null (probe-file file)))
    (with-open-file (out file :direction :output :element-type 'flex:octet
                              :if-does-not-exist :create)
      (let (http-stream
            size)
        (unwind-protect
             (with ((in _ headers (drakma:http-request
                                   (fmt "https://dumps.wikimedia.org/~(~A~)wiki/~
latest/~(~A~)wiki-latest-abstract.xml"
                                        lang lang)
                                   :want-stream t :force-binary t)))
               (:= size (parse-integer (assoc1 :content-length headers)))
               (:= http-stream in)
               (salza2:gzip-stream in out))
          (close http-stream))
        (values file
                size)))))


;;; Wiki XML

(defclass defs-extractor (sax:sax-parser-mixin)
  ((lang :initarg :lang)
   (tag :initform nil)
   (track :initform nil)
   (text :initform ())
   (anchors :initform #h(equalp))
   (words :initform #h(equalp))
   (3gs :initform #h(equalp))
   (tagcount :initform 0)
   (pagecount :initform 0)
   (outfile :initarg :outfile)
   (script :initarg :script :accessor sax-script :initform nil)))

(defmethod sax:start-document ((sax defs-extractor))
  ;; do nothing
  )

(defmethod sax:start-element ((sax defs-extractor)
                              namespace-uri local-name qname attributes)
  "Record in SAX object the tag of current element for future use.
   Print progress to *standard-output*."
  (:= @sax.tag (mkeyw local-name))
  (when (zerop (2nd (floor (incf @sax.tagcount) 10000)))
    (format t ".")))

(defmethod sax:characters ((sax defs-extractor) data)
  "Either record the word, found in <title> element (if it's legitimate)
   or collect parts of strings from the <text> element
   (it can be split in parts, if there are some HTML entities inside it)."
  (when (eql :abstract @sax.tag)
     (push data @sax.text)
     (:+ @sax.pagecount)))

(defmethod sax:end-element ((sax defs-extractor) namespace-uri local-name qname)
  "Record ngrams with their frequencies
   from Wiktionary definitions found inside <text> elements."
  (when @sax.text
    (let ((raw (fmt "~{~A~}" (reverse @sax.text))))
      (unless (blankp (string-trim (fmt " ~%") raw))
        (write-line raw @sax.outfile)))
    (void @sax.text)))
          
(defmethod sax:end-document ((sax defs-extractor))
  ;; TODO
  )

(defmethod sax:start-prefix-mapping ((sax defs-extractor) _ __)
  ;; dummy - do nothing
  )

(defmethod sax:end-prefix-mapping ((sax defs-extractor) _)
  ;; dummy - do nothing
  )


;;; dumps processing

(defun file-lang (file)
  "Determine the languge of wiki FILE from its name."
  (mkeyw (slice (pathname-name file) 0
                (search "wiki" (pathname-name file) :test 'string=))))

(defun process-wiki-file (file)
  "Process a single wikipedia dump FILE.
   Returns 4 values:
   - table of word logprobs
   - total number of words
   - table of char 3grams logprobs
   - total number of char 3grams"
  (let ((lang (file-lang file)))
    (with-gzip-file (out (merge-pathnames (fmt "../defs/~A.txt.gz" lang)
                                          file)
                         :direction :output)
      (let ((sax (make 'defs-extractor :outfile (flex:make-flexi-stream
                                                 out :external-format :utf-8)
                                       :lang lang)))
        (handler-case
            (with-open-file (in file :element-type 'flex:octet)
              (handler-bind ((cxml:well-formedness-violation
                               (lambda (e)
                                 (declare (ignore e))
                                 (when-it (or (find-restart 'cxml::ignore-namespace)
                                              (find-restart 'cxml::ignore-double-attr)
                                              (find-restart 'cxml::close-end-tag)
                                              (find-restart 'cxml::skip-end-tag)
                                              (find-restart 'cxml::treat-<-as-lt)
                                              (find-restart 'cxml::treat-single-&-as-is)
                                              (find-restart 'cxml::finish-processing))
                                   (invoke-restart it)))))
                (cxml:parse (flex:make-flexi-stream
                             (chipz:make-decompressing-stream 'chipz:gzip in)
                             :external-format :utf-8)
                            sax)))
          (error (e) (error e)
            (warn "Error processing ~A" file)
            (sax:end-document sax)))))))

(defun junk-defs-line (line)
  "Check if LINE is not eligible for use as a source of words
   (because it contains some service information in English)."
  (or (blankp line)
      (member (char line 0) '(#\| #\[))
      (re:scan "^\\d+px" line)
      (starts-with "thumb|" line)
      (starts-with "right|" line)
      (starts-with "{|" line)
      (not (find #\Space line))))

(defun process-abstracts-file (lang file &optional (detector *lang-detector*))
  "Process a gzipped FILE with abstracts for a given LANG
   and add its words and char 3grams to DETECTOR model.
   Return a list of 10 test lines, extracted from the same file
   and a total count of lines, which were processed and added to the model."
  (let ((lines #h(equal))
        (words #h(equal))
        (3gs #h(equal))
        (words-count 0)
        (3gs-count 0)
        (test-lines-req 10)
        test-lines)
    (when (probe-file file)
      (with-gzip-file (in file)
        (let ((in (flex:make-flexi-stream in :external-format :utf-8)))
          (loop :for line := (read-line in nil) :while line :do
            (:+ (get# (re:regex-replace-all "<[^>]+>" line "")
                      lines 0)))))
      (:= (? @detector.langs lang) t)
      (dotable (line count lines)
        (when (or (> count 5)
                  (junk-defs-line line))
          (rem# line lines)))
      (let ((test-idxs (print (maptimes (* 5 test-lines-req)
                                 ^(random (ht-count lines)))))
            (i 0)
            ok-line)
        (dotable (line _ lines)
          ;; leave only script-consistent lines
          (let ((scripts #h(equal)))
            (dovec (char line)
              (:+ (get# (unicode-script char) scripts 0)))
            (dotable (script cnt scripts)
              (when (> cnt (floor (length line) 2))
                (:= ok-line t)
                (return)))
            (when ok-line
              (if (and (< (length test-lines) test-lines-req)
                       (member i test-idxs)
                       (> (length line) 30))
                  (push line test-lines)
                  (let ((cur-words (remove-if ^(in# (unicode-script (char % 0))
                                                    *1lang-scripts*)
                                              (extract-words line))))
                    (:+ words-count (length cur-words))
                    (dolist (word cur-words)
                      (:+ (get# word words 0))
                      (let ((cur-3gs (extract-3gs word)))
                        (:+ 3gs-count (length cur-3gs))
                        (dovec (3g cur-3gs)
                          (:+ (get# 3g 3gs 0))))))))
            (:+ i))))
      (:+ @detector.words-count words-count)
      (:+ @detector.3gs-count 3gs-count)
      (dotable (word count words)
        (vector-push-extend (pair lang (log (/ count words-count)))
                            (get# word @detector.words vec)))
      (dotable (3g count 3gs)
        (vector-push-extend (pair lang (log (/ count 3gs-count)))
                            (get# 3g @detector.word3gs vec))))
    (values test-lines
            (ht-count lines))))

(defun process-wiki-defs ()
  "Process all wikipedia definitions."
  (doindex (i lang (mapcar 'lt (ht->pairs *iso-639-1*)))
    (print lang)
    (unless (member lang (ht-vals *1lang-scripts*))
      (let ((test-lines (process-abstracts-file
                         lang (fmt "data/defs/~A.txt.gz" lang))))
        (terpri)
        (when test-lines
          (with-out-file (out (fmt "data/test/~A.txt" lang))
            (dolist (line test-lines)
              (write-line line out))))))))
