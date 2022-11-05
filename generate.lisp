(in-package :cl-user)

(defpackage :my-blog
  (:use cl iterate spinneret)
  (:export :generate-site))

(in-package :my-blog)

(defparameter *src-directory* (uiop/os:getcwd))

(defparameter *dest-directory* (merge-pathnames "dist/" *src-directory*))

(defparameter 3bmd-code-blocks:*code-blocks* t)
(defparameter 3bmd-code-blocks:*renderer* :nohighlight)

(defparameter +front-matter-regex+
  (ppcre:create-scanner "---[\\n\\r](.*)[\\n\\r]---[\\n\\r]" :single-line-mode t))

(defun parse (string)
  "Parses front matter from a string, returning two values: the text of the front matter and the file's content."
  (multiple-value-bind (start end)
      (ppcre:scan +front-matter-regex+ string)
    (values (subseq string (+ 4 start) (- end 5))
            (subseq string (1+ end)))))


;;;;;;;;;;;;
;; LAYOUT ;;
;;;;;;;;;;;;

(deftag my-header (control attrs)
  `(:header#sidebar
    (:nav :role "navigation"
     :aria-label "main-navigation"
     (:a :href "/" "about me")
     (:a :href "/now.html" "now")
     (:a :href "/notes.html" "notes")
     (:a :href "/media/cv.pdf" "résumé"))))

(defparameter *footer-content*
  (with-open-file (in (merge-pathnames "templates/footer.html" *src-directory*))
    (uiop:read-file-string in)))

(deftag my-footer (control attrs)
  `(:footer
    (:raw *footer-content*)))

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
         (my-footer)))))))

(deftag page-header (control attrs)
  `(:section.hero
    (:p.title ,@control)))

(defmacro with-page ((&key title) &body body)
  `(with-master ()
     (page-header ,title)
     ,@body))


;;;;;;;;;;;;;;;;;;;;;
;; PAGE GENERATORS ;;
;;;;;;;;;;;;;;;;;;;;;


(defun read-md-as-html (path)
  (let ((content (with-open-file (file (merge-pathnames path *src-directory*))
                   (uiop:read-file-string file))))
    (with-output-to-string (s)
      (3bmd:parse-string-and-print-to-stream content s))))

(defun index-page ()
  (with-master ()
    (:raw (read-md-as-html "pages/index.md"))))

(defun portfolio-page ()
  (with-page (:title "Portfolio")
    (:raw (read-md-as-html "pages/portfolio.md"))))

(defun now-page ()
  (with-page (:title "My Now Page")
    (:raw (read-md-as-html "pages/now.md"))))

;;;;;;;;;;
;; NOTE ;;
;;;;;;;;;;

(defstruct note
  title
  created
  updated
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
                                  :created (local-time:parse-timestring (gethash "created" frontmatter))
                                  :updated (local-time:parse-timestring (gethash "updated" frontmatter))
                                  :link (get-note-link base-path)
                                  :dest-file (merge-pathnames base-path *dest-directory*)
                                  :content content)))))))

;;;;;;;;;;;;;;;;;;
;; STATIC FILES ;;
;;;;;;;;;;;;;;;;;;

(defparameter *static-files*
  '("fonts/Charter Bold.otf"
    "fonts/Charter Bold.ttf"
    "fonts/Charter Bold.woff2"
    "fonts/Charter Regular.otf"
    "fonts/Charter Regular.ttf"
    "fonts/Charter Regular.woff2"
    "fonts/FiraMono-Regular.otf"
    "fonts/FiraMono-Regular.ttf"
    "fonts/FiraMono-Regular.woff"
    "fonts/FiraMono-Regular.woff2"
    "media/cv.pdf"
    "media/me.jpg"
    "style.css"
    "app.js"))

(defun copy-files ()
  (dolist (file *static-files*)
    (let ((src (merge-pathnames file *src-directory*))
          (dest (merge-pathnames file *dest-directory*)))
      (ensure-directories-exist dest)
      (uiop:copy-file src dest))))

;;;;;;;;;;;;;
;; WRITERS ;;
;;;;;;;;;;;;;

(defmacro write-page (dest &body body)
  `(with-open-file (*html* ,dest
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
     ,@body))

(defparameter *pages*
  '(("index.html" index-page)
    ("portfolio.html" portfolio-page)
    ("now.html" now-page)))

(defun generate-site ()
  (ensure-directories-exist *dest-directory*)
  (dolist (p *pages*)
    (write-page (merge-pathnames (first p) *dest-directory*)
                (apply (second p) nil)))
  (let ((notes (sort (find-notes)
                     #'local-time:timestamp>
                     :key (lambda (note) (note-updated note)))))
    (write-page (merge-pathnames "notes.html" *dest-directory*)
                (notes-page notes))
    (dolist (note notes)
      (ensure-directories-exist (note-dest-file note))
      (write-page (note-dest-file note)
                  (note-page note))))
  (copy-files))
