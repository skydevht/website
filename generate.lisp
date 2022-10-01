(in-package :cl-user)

(defpackage :my-blog
  (:use cl)
  (:import-from :3bmd)
  (:import-from :yaml)
  (:import-from :access
                :access
                :accesses)
  (:import-from :djula
                :add-template-directory
                :compile-template*
                :render-template*)
  (:import-from :my-blog.parse-front-matter :parse)
  (:export :generate))


(in-package :my-blog)

(setf *default-pathname-defaults* (uiop/os:getcwd))

(defparameter +config+ 
  (with-open-file (in (merge-pathnames "config.yml"))
    (yaml:parse (uiop:read-file-string in))))

(defparameter +dist-directory+
  (merge-pathnames (accesses +config+ "build" "directory")))


(defparameter +template-directory+
  (merge-pathnames (accesses +config+ "templates")))

(djula:add-template-directory +template-directory+)

(defparameter +notes-directory+ (merge-pathnames "notes/"))

(defparameter +notes-dist-directory+ (merge-pathnames "notes/" +dist-directory+))


(defparameter +content-types+ (access +config+ "content-types"))

(defparameter +notes-html+ 
  (compile-template* (accesses (first +content-types+) 
                               "list"
                               "template")))

(defparameter +note-html+ 
  (compile-template* (accesses (first +content-types+) 
                               "single"
                               "template")))

(defun ls-notes ()
  (uiop:directory-files +notes-directory+))

(defun render-note (file)
  (multiple-value-bind (front content)
      (parse (uiop:read-file-string file))
    (values (with-output-to-string (s)
              (3bmd:parse-string-and-print-to-stream content s))
            (yaml:parse front)
            file)))

(defun dest-note-pathname (src)
  (merge-pathnames (concatenate 'string (pathname-name src) ".html") +notes-dist-directory+))

(defun write-note (content meta file)
  (with-open-file (in (dest-note-pathname file) 
                      :direction :output
                      :if-does-not-exist :create
                      :if-exists :overwrite)
    (render-template* +note-html+ in 
                      :content content 
                      :title (gethash "title" meta ""))))

(defun get-notes ()
  (mapcar (lambda (file) (multiple-value-list (render-note file))) (ls-notes)))

(defun note-link (file)
  (concatenate 'string "./notes/" (pathname-name file) ".html"))

(defun note-for-template (note-as-list)
  (let ((note (make-hash-table)))
    (setf (access note :title) (access (second note-as-list) "title"))
    (setf (access note :link) (note-link (third note-as-list)))
    note))

(defun write-notes ()
  (let ((notes (get-notes)))
    (with-open-file (in (merge-pathnames "notes.html" +dist-directory+)
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :overwrite)
      (render-template* +notes-html+ 
                        in 
                        :notes (mapcar #'note-for-template notes)))
    (dolist (note notes)
      (destructuring-bind (content meta file) note
          (write-note content meta file)))))


(defun write-page (page)
  (with-open-file (in (merge-pathnames page +dist-directory+)
                      :direction :output
                      :if-does-not-exist :create
                      :if-exists :overwrite)
    (render-template* (compile-template* page) in)))

(defun build-path (path)
  (concatenate 'string (accesses +config+ "build" "directory") path))

(defun copy-files () 
    (dolist (file (access +config+ "static-files"))
      (let ((src (access file "src"))
            (dest (build-path (access file "dest"))))
        (ensure-directories-exist dest)
        (uiop:copy-file src dest))))

(defun generate ()
  (ensure-directories-exist +dist-directory+)
  (dolist (page '("index.html" "portfolio.html"))
    (write-page page))
  (ensure-directories-exist +notes-dist-directory+)
  (write-notes)
  (copy-files))
