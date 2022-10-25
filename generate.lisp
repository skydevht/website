(in-package :cl-user)

(defpackage :my-blog
  (:use cl iterate spinneret)
  (:import-from :my-blog.parse-front-matter :parse)
  (:export :generate-site))

(in-package :my-blog)

(defparameter *src-directory* (uiop/os:getcwd))

(defparameter *dest-directory* (merge-pathnames "dist/" *src-directory*))

(deftag my-header (control attrs)
    `(:header
	 (:nav :role "navigation"
	       :aria-label "main-navigation"
	  (:a :href "/" "about me")
	  (:a :href "/portfolio.html" "portfolio")
	  (:a :href "/notes.html" "notes")
	  (:a :href "/media/cv.pdf" "resume"))))

(defparameter *footer-content*
  (with-open-file (in (merge-pathnames "templates/footer.html" *src-directory*))
    (uiop:read-file-string in)))

(deftag my-footer (control attrs)
  `(:footer ,@control))

(defmacro with-master (&body body)
  `(with-html
     (:doctype)
     (:html
      (:head
       (:title "Hi, I'm Holy-Elie Scaide")
       (:meta :name "viewport"
	      :content "width=device-width, initial-scale=1, shrink-to-fit=no")
       (:link :rel "stylesheet" :href "/style.css"))
      (:body
       (:main
	(my-header)
	(:article
	 ,@body
	 (my-footer (:raw ,*footer-content*))))))))

(deftag page-header (control attrs)
  `(:section.hero
    (:p.title ,@control)))

(defmacro with-page ((&key title) &body body)
  `(with-master ()
     (page-header ,title)
     ,@body))

(defparameter *index-content*
  (with-open-file (in (merge-pathnames "templates/index.html" *src-directory*))
    (uiop:read-file-string in)))

(defun index-page ()
  (with-master ()
     (:raw *index-content*)
    ))

(defparameter *portfolio-content*
  (with-open-file (in (merge-pathnames "templates/portfolio.html" *src-directory*))
		  (uiop:read-file-string in)))

(defun portfolio-page ()
  (with-page (:title "Portfolio")
    (:raw *portfolio-content*)))

(defstruct note
  title
  dest-file
  content
  link)

(defun notes-page (notes)
  (with-page (:title "Notes")
    (:p "Below are all my notes in no particular order.")
    (:ul (dolist (note notes)
	   (:li
	    (:a :href (note-link note) (note-title note)))))))

(defun note-page (note)
  (with-page (:title (note-title note))
    (:raw (note-content note))))


(defun get-note-dest-base (src)
  (concatenate 'string "notes/" (pathname-name src) ".html"))

(defun get-note-link (base-path)
  (concatenate 'string "/" base-path))

(defun find-notes ()
  (let ((note-files (uiop:directory-files (merge-pathnames "notes/" *src-directory*))))
    (iter (for note-file in note-files)
      (collect (multiple-value-bind (fm-data markdown)
		   (parse (uiop:read-file-string note-file))
		 (let ((content (with-output-to-string (s)
				  (3bmd:parse-string-and-print-to-stream markdown s)))
		       (frontmatter (yaml:parse fm-data))
		       (base-path (get-note-dest-base note-file)))
		   (make-note :title (gethash "title" frontmatter "")
			      :link (get-note-link base-path)
			      :dest-file (merge-pathnames base-path *dest-directory*)
			      :content content)))))))

(defparameter *static-files*
  '("fonts/Charter Bold.otf"
    "fonts/Charter Regular.otf"
    "fonts/FiraMono-Regular.woff"
    "media/cv.pdf"
    "style.css"
    "app.js"))

(defun copy-files ()
  (dolist (file *static-files*)
    (let ((src (merge-pathnames file *src-directory*))
	  (dest (merge-pathnames file *dest-directory*)))
      (ensure-directories-exist dest)
      (uiop:copy-file src dest))))

(defmacro write-page (dest &body body)
  `(with-open-file (*html* ,dest
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
     ,@body))

(defparameter *pages*
  '(("index.html" index-page)
    ("portfolio.html" portfolio-page)))

(defun generate-site ()
  (ensure-directories-exist *dest-directory*)
  (dolist (p *pages*)
    (write-page (merge-pathnames (first p) *dest-directory*)
      (apply (second p) nil)))
  (let ((notes (find-notes)))
    (write-page (merge-pathnames "notes.html" *dest-directory*)
      (notes-page notes))
    (dolist (note notes)
      (ensure-directories-exist (note-dest-file note))
      (write-page (note-dest-file note)
	(note-page note))))
  (copy-files))
