;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          base64.asd
;;;; Purpose:       ASDF definition file for Base64
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Dec 2002
;;;;
;;;; $Id: base64.asd,v 1.20 2003/05/06 16:19:51 kevin Exp $
;;;; *************************************************************************

(cl:defpackage #:base64-system (:use #:asdf #:cl))
(cl:in-package #:base64-system)


(defsystem :base64
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
   (:file "base64-tests" :depends-on ("encode" "decode"))
   ))

(defmethod perform ((o test-op) (c (eql (find-system :base64))))
  (or (funcall (intern (symbol-name '#:test-base64) (find-package 'base64-test)))
      (error "test-op failed")))
