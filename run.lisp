(load "../wiki-lang-detect/init.lisp")

(format t "\nService runs at http://localhost:5000/\n")
(bt:join-thread wild:*woo*)
