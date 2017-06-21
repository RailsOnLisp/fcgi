
(in-package :common-lisp-user)

(defpackage :fcgi.system
  (:use :common-lisp :asdf))

(in-package :fcgi.system)

(defsystem "fcgi"
  :depends-on ("cffi-sockets" "flexi-streams")
  :components
  ((:file "package")
   (:file "fcgi" :depends-on ("package"))))
