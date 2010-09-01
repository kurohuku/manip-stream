;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-

(asdf:defsystem #:manip-stream
  :version "0.0.1"
  :components ((:file "packages")
	       (:file "manip-stream"))
  :depends-on (:trivial-gray-streams))
