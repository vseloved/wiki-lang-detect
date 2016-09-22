(in-package #:wiki-lang-detect)
(named-readtables:in-readtable rutilsx-readtable)


(defparameter *text-length-limit* 5000
  "The limit on the number of characters in the request.")

#+nil
(url "/detect" ()
  (case (htt:request-method*)
    (:POST
     (:= (htt:content-type*) "application/json")
     (let ((text (assoc1 :text (json:decode-json (htt:raw-post-data
                                                  :force-text t :want-stream t
                                                  :external-format :utf8)))))
       (if (> (length text) *text-length-limit*)
           (abort-request htt:+http-not-acceptable+)
           (json:encode-json-to-string (text-langs text)))))
    (otherwise
     (hunch:abort-request htt:+http-bad-request+))))

(defparameter *swagger-def*
  (json:encode-json-to-string
   #h(equal
      "swagger" "2.0"
      "basePath" "/"
      "paths"
      #h(equal
         "/detect"
         #h(equal
            "post" #h(equal
                      "produces" '("application/json")
                      "parameters"
                      (list
                       #h(equal
                          "name" "text"
                          "in" "body"
                          "required" t
                          "type" "string"
                          "maxLength" 5000))
                      "responses"
                      #h(equal
                         "500" "Internal server error"
                         "400" "Bad request"
                         "200" #h(equal
                                  "examples" #h(equal
                                                "application/json"
                                                #h(equal "en" 1.0))
                                  "description" "Result of language detection"
                                  "schema" #h(equal
                                              "type" "object"))))
            "summary" "Detect the language of the provided text"
            "description" "The language is detecrmined based on words and character trigrams probabilities"
            "tags" '("langid" "lang-uk")
            "consumes" '("application/json")
            "x-microservice-taxonomy" '("test")))
      "info" #h(equal
                "version" "0.9.0"
                "contact" #h(equal
                             "name" "Vsevolod Dyomkin"
                             "email" "vseloved@gmail.com")
                "description" "Language identification for 156 languages"
                "license" #h(equal
                             "name" "Apache")
                "titel" "wiki-lang-detect")
      "schemes" '("http")
      "host" "wild.lisp.kiev.ua")))

(defun woo-api (req)
  (let ((path (getf req :path-info))
        (method (getf req :request-method)))
    (format *standard-output* "~A ~A ~A" (local-time:now) method path)
    (cond
      ((starts-with "/swagger" path)
       (list 200 '(:content-type "application/json")
             (list *swagger-def*)))
      ((and (starts-with "/detect" path)
            (eql :POST method))
       (handler-case
           (let ((text (assoc1 "text" (http-body:parse 
                                       (getf req :content-type)
                                       (getf req :content-length)
                                       (getf req :raw-body))
                               :test 'string-equal)))
             (if (> (length text) *text-length-limit*)
                 (list 400 '(:content-type "text/plain")
                       (list (fmt "Text size exceeds maxLength (~A): ~A"
                                  *text-length-limit* (length text))))
                 (list 200 '(:content-type "application/json")
                       (list (json:encode-json-to-string (text-langs text))))))
         (error (e)
           (format *error-output* "~A" e)
           '(500 nil nil))))
      (t '(404 nil nil)))))
