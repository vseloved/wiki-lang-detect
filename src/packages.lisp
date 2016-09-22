(cl:defpackage #:wiki-lang-detect
  (:nicknames #:wild)
  (:use :common-lisp #:rutilsx
        #+dev #:should-test
        #+prod #:hunch)
  (:export #:word-langs
           #:word-direct-langs
           #:text-lang
           #:text-langs

           #:*iso-639-1*
           #:iso-lang
           #:lang-iso

           #:lang-detector           
           #:*lang-detector*

           #:*1lang-scripts*
           #:*min-lang-bias*

           #:save-model
           #:load-model))


(in-package #:wiki-lang-detect)

(rename-package "CL-PPCRE" "CL-PPCRE" '("PPCRE" "RE"))

(abbr with-gzip-file gzip-stream:with-open-gzip-file)

#+dev
(rutil:eval-always
  (:= *default-pathname-defaults*
      (fad:pathname-directory-pathname
       (asdf:component-pathname (asdf:find-system '#:wiki-lang-detect)))))
