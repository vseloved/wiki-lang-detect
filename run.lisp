(push :prod *features*)

(push (merge-pathnames "../" *load-truename*) asdf:*central-registry*)

(ql:quickload :wiki-lang-detect)
