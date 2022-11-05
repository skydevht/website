(defsystem "my-blog"
  :version "0.1.0"
  :author "Holy-Elie Scaide"
  :depends-on ("3bmd"
               "3bmd-ext-code-blocks"
               "iterate"
               "spinneret"
               "local-time"
               "cl-yaml")
  :license "MIT"
  :components ((:file "generate")))

; vim: ft=lisp
