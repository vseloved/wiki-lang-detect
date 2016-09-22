(cl:in-package #:wild)
(named-readtables:in-readtable rutilsx-readtable)



(deftest extract-words ()
  (should be equal '("this" "is" "a" "tеst")
          (extract-words "this is a tеst"))
  (should be equal '("this" "위키백과" "is" "a" "tеst")
          (extract-words "this위키백과 is a tеst"))
  (should be equal '("this" "위키" "a" "백과" "is" "a" "tеst")
          (extract-words "this위키a백과 is a tеst"))
  (should be null
          (extract-words ""))
  (should be equal '("aaa")
          (extract-words "aaa"))
  (should be equal '("aaa111" "222")
          (extract-words "aaa111 222")))
