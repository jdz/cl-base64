;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          test.lisp
;;;; Purpose:       Regression tests for cl-base64
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Jan 2003
;;;;
;;;; $Id: base64-tests.lisp,v 1.2 2003/04/15 15:44:07 kevin Exp $
;;;; *************************************************************************

(in-package :cl-user)

(defpackage #:base64-test
  (:use #:cl #:kmrcl #:base64 #:util.test))

(in-package #:base64-test)

(defun test-base64 ()
  (setq *break-on-test-failures* t) 
  (with-tests (:name "cl-base64 tests")
    (do* ((length 0 (+ 3 length))
	  (string (make-string length) (make-string length))
	  (usb8 (make-usb8-array length) (make-usb8-array length))
	  (integer (random (expt 10 length)) (random (expt 10 length))))
	 ((>= length 300))
    (dotimes (i length)
      (declare (fixnum i))
      (let ((code (random 256)))
	(setf (schar string i) (code-char code))
	(setf (aref usb8 i) code)))

      (do* ((columns 0 (+ columns 4)))
	   ((> columns length))
      ;; Test against cl-base64 routines
	(test integer (base64-string-to-integer
				 (integer-to-base64-string integer :columns columns)))
	(test string (base64-string-to-string
				(string-to-base64-string string :columns columns))
		      :test #'string=)
      
      ;; Test against AllegroCL built-in routines
      #+allegro
      (progn
      (test integer (excl:base64-string-to-integer
			       (integer-to-base64-string integer :columns columns)))
      (test integer (base64-string-to-integer
			       (excl:integer-to-base64-string integer)))
      (test (string-to-base64-string string :columns columns)
	    (excl:usb8-array-to-base64-string usb8
					      (if (zerop columns)
						  nil
						  columns))
	    :test #'string=)
      (test string (base64-string-to-string
		    (excl:usb8-array-to-base64-string
		     usb8
		     (if (zerop columns)
			 nil
			 columns)))
	    :test #'string=))))))


(defun time-routines ()
  (let* ((str "abcdefghijklmnopqwertyu1234589jhwf2ff")
	 (usb8 (string-to-usb8-array str))
	 (int 12345678901234567890)
	 (n 50000))
    (time-iterations n (integer-to-base64-string int))
    (time-iterations n (excl:integer-to-base64-string int))
    (time-iterations n (string-to-base64-string str))
    (time-iterations n (excl:usb8-array-to-base64-string usb8))))

      
;;#+run-test (test-base64)
