(cl:in-package #:wild)
(named-readtables:in-readtable rutilsx-readtable)


(deftest smoke-test ()
  (eql :en (detect-lang "the" :priors #h(:en #h(equalp "the" 1)
                                         :de #h(equalp "the" 0.2)
                                         :ru #h(equalp)))))

(deftest comprehensive-test ()
  (every #'identity
         (mapcan #`(let ((cur (read-file (in-project-dir
                                          (fmt "../test/texts/~(~A~).txt" %)))))
                     (format *test-out* "~&LANG: ~A.txt~%" %)
                     (cons (eql % (detect-lang cur))
                           (mapcar (lambda (text)
                                     (let ((lang (detect-lang text)))
                                       (format *test-out* "~&~A - ~A~%"
                                               lang text)
                                       (eql % lang)))
                                   (get-random-snippets cur))))
                 *langs*)))

(defun get-random-snippets (text &optional (n 10))
  (let (rez)
    (dotimes (i n (reverse rez))
      (let ((start (random (length text))))
        (push (subseq text start (min (+ start (* i 5))
                                      (length text)))
              rez)))))
