;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          base64.asd
;;;; Purpose:       ASDF definition file for Base64
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Dec 2002
;;;;
;;;; $Id: base64.asd,v 1.1 2002/12/29 06:08:15 kevin Exp $
;;;; *************************************************************************

(in-package :asdf)

(defsystem :base64
  :name "cl-base64"
  :author "Kevin M. Rosenberg and Juri Pakaste"
  :version "1.0"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Public domain"
  :description "Base64 encode and decoding"
  
  :perform (load-op :after (op base64)
	    (pushnew :base64 cl:*features*))
  
  :components
  ((:file "base64")))
