(in-package #:wiki-lang-detect)
(named-readtables:in-readtable rutilsx-readtable)


(defvar *lang-detector*
  (let ((model-file (merge-pathnames "models/wiki156micro.zip"
                                     (asdf:component-pathname
                                      (asdf:find-system :wiki-lang-detect)))))
    (if (probe-file model-file)
        (huffman-model (load-model model-file))
        (warn "No model at ~A" model-file)))
  "Default language detector.")

(dolist (script '(nil "Common" "Canadian_Aboriginal" "Tifinagh" "Old Persian"
                  "Gothic" "Inherited" "Cuneiform" "Javanese" "Ugaritic"
                  "Mongolian" "Syriac" "Linear_B" "Buginese"))
  (rem# script @*lang-detector*.3gs)
  (rem# script @*lang-detector*.words))

#+prod
(defparameter *woo* (bt:make-thread ^(woo:run 'woo-api :port 5000)))
