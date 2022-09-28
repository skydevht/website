(in-package :cl-user)

(defpackage :my-blog.build
  (:use cl)
  (:import-from :cl-markdown)
  (:import-from :yaml)
  (:import-from :my-blog.parse-front-matter :parse)
  (:export :build))

(in-package :my-blog.build)

(defparameter *notes-directory* 
  (asdf:system-relative-pathname "my-blog" "notes/"))

(defparameter *template-directory* 
  (asdf:system-relative-pathname "my-blog" "pages/"))

(defun ls-notes ()
  (uiop:directory-files *notes-directory*))

(defun render (file)
  (multiple-value-bind (front content)
      (parse (uiop:read-file-string file))
    (values (cl-markdown:markdown content :stream nil)
            (yaml:parse front))))

(render (first (ls-notes)))
