
(in-package :common-lisp)

(defpackage :fcgi
  (:use
   :cffi
   :cffi-sockets
   :common-lisp
   :flexi-streams
   :trivial-gray-streams)
  (:export :socket-server))
