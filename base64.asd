;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          base64.asd
;;;; Purpose:       ASDF definition file for Base64
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Dec 2002
;;;;
;;;; $Id: base64.asd,v 1.2 2002/12/29 06:14:49 kevin Exp $
;;;; *************************************************************************

(in-package :asdf)

(defsystem :base64
  :name "cl-base64"
  :author "Kevin M. Rosenberg based on code by Juri Pakaste"
  :version "1.0"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "BSD-style"
  :description "Base64 encoding and decoding with URI support."
  
  :perform (load-op :after (op base64)
	    (pushnew :base64 cl:*features*))
  
  :components
  ((:file "src")))
