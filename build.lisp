(ql:quickload :my-blog)

(sb-ext:save-lisp-and-die "generator"
 :toplevel 'my-blog:generate-site
 :executable t)
