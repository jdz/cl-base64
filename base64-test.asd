;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          base64-test.asd
;;;; Purpose:       ASDF definition file for Base64 Regression Test
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Jan 2003
;;;;
;;;; $Id: base64-test.asd,v 1.1 2003/01/12 20:25:26 kevin Exp $
;;;; *************************************************************************

(in-package :asdf)

#+allegro (require 'tester)

(defsystem :base64-test
  :name "cl-base64-test"
  :author "Kevin M. Rosenberg based on code by Juri Pakaste"
  :version "1.0"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "BSD-style"
  :description "Regression test for cl-base64 package"
  
  :depends-on (:base64 :kmrcl #-allegro :tester)  
  :components
  ((:file "test")))
