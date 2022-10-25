(defsystem "my-blog"
  :version "0.1.0"
  :author "Holy-Elie Scaide"
  :depends-on ("3bmd"
	       "iterate"
	       "spinneret"
	       "cl-yaml")
  :license "MIT"
  :components ((:file "generate" :depends-on ("parse-front-matter"))
	       (:file "parse-front-matter")))

; vim: ft=lisp
