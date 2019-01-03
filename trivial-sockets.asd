;;; -*- Lisp -*-
(defpackage #:trivial-sockets-system  (:use #:asdf #:cl))
(in-package #:trivial-sockets-system )

(defsystem trivial-sockets
    :version "0.4"
    :author "Daniel Barlow"
    :maintainer "Chun Tian (binghe)"
    :licence "MIT"
    :description "A trivial networking library for undemanding Internet applications"
    :depends-on (#+sbcl sb-bsd-sockets)
    :components ((:file "defpackage")
		 (:file "errors"  :depends-on ("defpackage"))
		 (:file 
		  #+sbcl "sbcl" 
		  #+cmu "cmucl"
		  #+clisp "clisp"
		  #+acl-socket "allegro"
		  #+openmcl "openmcl"
		  #+lispworks "lispworks"
		  #+armedbear "abcl"
                  #+Digitool "mcl"
		  :depends-on ("defpackage"))
		 (:file "server" :depends-on ("defpackage"))
		 ))

