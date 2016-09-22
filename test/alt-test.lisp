(cl:in-package #:wild)
(named-readtables:in-readtable rutilsx-readtable)


(defun test-file-langid (file from-twitter)
  (let ((tmp-file (fmt "/tmp/~A.txt" (gensym))))
    (unwind-protect
         (progn
           (with-out-file (out tmp-file)
             (dolines (line file)
               (when (> (length line) 20)
                 (when from-twitter
                   (:= line (re:regex-replace-all "@\\S+" line "")))
                 (write-line line out))))
           (uiop:chdir (merge-pathnames "alt/langid/langid/"))
           (with-open-file (in file)
             (loop :for line := (read-line in nil) :while line
                   :for rez-line :in (split #\Newline
                                            (with-output-to-string (out)
                                              (uiop:run-program
                                               '("python" "langid.py" "-n" "--line")
                                               :input tmp-file :output out))
                                            :remove-empty-subseqs t)
                   :collect (cons line (list (cons (mkeyw (subseq rez-line 2 4))
                                                   1.0))))))
      (delete-file tmp-file))))

(defun test-file-cld2 (file from-twitter)
  (unless (uiop:getenv "LD_LIBRARY_PATH")
    (:= (uiop:getenv "LD_LIBRARY_PATH")
        (princ-to-string (merge-pathnames "alt/cld2/internal/"))))
  (let ((tmp-file (fmt "/tmp/~A.txt" (gensym))))
    (unwind-protect
         (progn
           (with-out-file (out tmp-file)
             (dolines (line file)
               (when (> (length line) 20)
                 (when from-twitter
                   (:= line (re:regex-replace-all "@\\S+" line ""))
                   (:= line (subseq line 0 (position #\Space line
                                                     :start (1+ (or (position #\Space line)
                                                                    -1))))))
                 (write-line line out))))
           (uiop:chdir (merge-pathnames "alt/cld2/python-cld2/"))
           (with-open-file (in file)
             (loop :for line := (read-line in nil) :while line
                   :for rez-line :in (split #\Newline
                                            (with-output-to-string (out)
                                              (uiop:run-program
                                               (list "python" "batch.py" tmp-file)
                                               :output out))
                                            :remove-empty-subseqs t)
                   :collect (cons line (list (cons (if (string-equal "un" rez-line)
                                                       :und
                                                       (mkeyw (subseq rez-line 0 2)))
                                                   1.0))))))
      (delete-file tmp-file))))
