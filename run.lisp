(push :prod *features*)

(push (merge-pathnames "../wiki-lang-detect/" *load-truename*) asdf:*central-registry*)

(ql:quickload :wiki-lang-detect)
