(push :prod *features*)

(push (merge-pathnames "../wiki-lang-detect/") asdf:*central-registry*)

(ql:quickload :wiki-lang-detect)
