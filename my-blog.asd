(defsystem "my-blog"
  :version "0.1.0"
  :author "Holy-Elie Scaide"
  :depends-on ("cl-markdown"
               "cl-yaml")
  :license "MIT"
  :components ((:file "build" :depends-on ("parse-front-matter"))
               (:file "parse-front-matter")))

; vim: ft=lisp
