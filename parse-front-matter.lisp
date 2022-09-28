(in-package :cl-user)

(defpackage :my-blog.parse-front-matter
  (:use cl)
  (:export :parse)
  (:documentation "The front matter parsing package."))

(in-package :my-blog.parse-front-matter)

(defparameter +front-matter-regex+
  (ppcre:create-scanner "---[\\n\\r](.*)[\\n\\r]---[\\n\\r]" :single-line-mode t))

(defun parse (string)
  "Parses front matter from a string, returning two values: the text of the front matter and the file's content."
  (multiple-value-bind (start end)
      (ppcre:scan +front-matter-regex+ string)
    (values (subseq string (+ 4 start) (- end 5))
            (subseq string (1+ end)))))
