;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for cl-base64
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Dec 2002
;;;;
;;;; $Id: package.lisp,v 1.1 2003/01/12 20:25:26 kevin Exp $
;;;;
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))

(defpackage #:cl-base64
  (:nicknames #:base64)
  (:use #:cl)
  (:export #:base64-stream-to-integer
	   #:base64-string-to-integer
	   #:base64-string-to-string
	   #:base64-stream-to-string
	   #:base64-string-to-stream
	   #:base64-stream-to-stream
	   #:base64-string-to-usb8-array
	   #:base64-stream-to-usb8-array
	   #:string-to-base64-string
	   #:string-to-base64-stream
	   #:usb8-array-to-base64-string
	   #:usb8-array-to-base64-stream
	   #:stream-to-base64-string
	   #:stream-to-base64-stream
	   #:integer-to-base64-string
	   #:integer-to-base64-stream

	   ;; For creating custom encode/decode tables
	   #:*uri-encode-table*
	   #:*uri-decode-table*
	   #:make-decode-table
	   ))

(in-package #:cl-base64)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *encode-table*
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
  (declaim (type simple-string *encode-table*))
  
  (defvar *uri-encode-table*
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")
  (declaim (type simple-string *uri-encode-table*))
  
  (deftype decode-table () '(array fixnum (256)))

  (defun make-decode-table (encode-table)
    (let ((dt (make-array 256 :adjustable nil :fill-pointer nil
			  :element-type 'fixnum
			  :initial-element -1)))
      (loop for char of-type character across encode-table
	 for index of-type fixnum from 0 below 64
	 do (setf (aref dt (the fixnum (char-code char))) index))
      dt))
    
  (defvar *decode-table* (make-decode-table *encode-table*))
  
  (defvar *uri-decode-table* (make-decode-table *uri-encode-table*))
  
  (declaim (type decode-table *decode-table* *uri-decode-table*))
  
  (defvar *pad-char* #\=)
  (defvar *uri-pad-char* #\.)
  (declaim (type character *pad-char* *uri-pad-char*))
  )
