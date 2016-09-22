(in-package #:asdf-user)

(defsystem #:wiki-lang-detect
  :version (:read-file-line "version.txt" :at 0)
  :description "Text language detection based on Wikipedia data with 170 suported languages."
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :depends-on (#:rutilsx #:cl-unicode #:cl-ppcre #:cxml #:chipz #:gzip-stream
               #:zip #:babel #:flexi-streams #:drakma
               #+dev #:should-test #+dev #:local-time
               #+prod #:cl-json #+prod #:woo #+prod #:http-body #+prod #:local-time)
               ;; #+prod #:hunchentoot)
  :serial t
  :components
  ((:module "src" :serial t
    :components ((:file "packages")
                 (:file "iso-lang")
                 (:file "huffman")
                 (:file "model")
                 (:file "core")
                 #+train (:file "wiki")
                 ;; #+prod (:file "hunch")
                 #+prod (:file "api")
                 (:file "user")))
   #+dev
   (:module "test" :serial t
    :components ((:file "util-test")
                 (:file "quality-test")
                 (:file "alt-test")
                 (:file "api-test")))))
