;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          base64.asd
;;;; Purpose:       ASDF definition file for Base64
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Dec 2002
;;;;
;;;; $Id: base64.asd,v 1.23 2003/08/25 16:27:23 kevin Exp $
;;;; *************************************************************************

(in-package #:cl-user)
(defpackage #:base64-system (:use #:asdf #:cl))
(in-package #:base64-system)


(defsystem base64
  :name "cl-base64"
  :author "Kevin M. Rosenberg based on initial code by Juri Pakaste"
  :version "3.1"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "BSD-style"
  :description "Base64 encoding and decoding with URI support."
  
  ;; depends-on only needed for test-op
  :depends-on (:kmrcl) 
  
  :components
  ((:file "package")
   (:file "encode" :depends-on ("package"))
   (:file "decode" :depends-on ("package"))
   ))

(defmethod perform ((o test-op) (c (eql (find-system 'base64))))
  (operate 'load-op 'base64-tests)
  (operate 'test-op 'base64-tests :force t))

(defsystem base64-tests
    :depends-on (base64 ptester)
    :components
    ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system 'base64-tests))))
  (operate 'load-op 'base64-tests)
  (or (funcall (intern (symbol-name '#:do-tests)
		       (find-package 'base64-test)))
      (error "test-op failed")))
