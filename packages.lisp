;;; -*- Mode: Lisp; Syntax: COMMON-LISP; -*-

(in-package #:cl-user)

(defpackage :manip-stream
  (:use :cl :trivial-gray-streams)
  (:export
   stream-manipulator
   manip-stream
   manip-output-stream
   set-flag
   clear-flags
   set-fill
   set-width
   set-base-field
   +endl+
   +left+
   +right+
   <<))
