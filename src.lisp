;;;; This file implements the Base64 transfer encoding algorithm as
;;;; defined in RFC 1521 by Borensten & Freed, September 1993.
;;;; See: http://www.ietf.org/rfc/rfc1521.txt
;;;;
;;;; Based on initial public domain code by Juri Pakaste <juri@iki.fi>
;;;;
;;;; Extended by Kevin M. Rosenberg <kevin@rosenberg.net>:
;;;;   - .asd file
;;;;   - numerous speed optimizations
;;;;   - conversion to and from integers
;;;;   - Renamed functions now that supporting integer conversions
;;;;   - URI-compatible encoding using :uri key
;;;;
;;;; Copyright 2002-2003 Kevin M. Rosenberg
;;;; Permission to use with BSD-style license included in the COPYING file
;;;;
;;;; $Id: src.lisp,v 1.1 2002/12/29 06:14:49 kevin Exp $

(defpackage #:base64
  (:use #:cl)
  (:export #:base64-to-string #:base64-to-integer
	   #:string-to-base64 #:integer-to-base64))

(in-package #:base64)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *encode-table*
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
  (declaim (type simple-string *encode-table*))
  
  (defvar *uri-encode-table*
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")
  (declaim (type simple-string *uri-encode-table*))
  
  (deftype decode-table () '(simple-array fixnum (256)))

  (defvar *decode-table*
    (let ((da (make-array 256 :adjustable nil :fill-pointer nil
			  :element-type 'fixnum
			  :initial-element -1)))
      (loop for char of-type character across *encode-table*
	    for index of-type fixnum from 0 below 64
	    do (setf (aref da (the fixnum (char-code char))) index))
      da))
  
  (defvar *uri-decode-table*
    (let ((da (make-array 256 :adjustable nil :fill-pointer nil
			  :element-type 'fixnum
			  :initial-element -1)))
      (loop
       for char of-type character across *uri-encode-table*
       for index of-type fixnum from 0 below 64
       do (setf (aref da (the fixnum (char-code char))) index))
      da))
  
  (declaim (type decode-table *decode-table* *uri-decode-table*))
  
  (defvar *pad-char* #\=)
  (defvar *uri-pad-char* #\.)
  (declaim (type character *pad-char* *uri-pad-char*))
  )

(defun string-to-base64 (string &key (uri nil))
  "Encode a string array to base64."
  (declare (string string)
	   (optimize (speed 3)))
  (let ((pad (if uri *uri-pad-char* *pad-char*))
	(encode-table (if uri *uri-encode-table* *encode-table*)))
    (declare (simple-string encode-table)
	     (character pad))
    (let* ((string-length (length string))
	   (result (make-string
		    (* 4 (truncate (/ (+ 2 string-length) 3))))))
      (declare (fixnum string-length)
	       (simple-string result))
      (do ((sidx 0 (the fixnum (+ sidx 3)))
	   (didx 0 (the fixnum (+ didx 4)))
	   (chars 2 2)
	   (value 0 0))
	  ((>= sidx string-length) t)
	(declare (fixnum sidx didx chars value))
	(setf value (ash (logand #xFF (char-code (char string sidx))) 8))
	(dotimes (n 2)
	  (declare (fixnum n))
	  (when (< (the fixnum (+ sidx n 1)) string-length)
	    (setf value
		  (logior value
			  (the fixnum
			    (logand #xFF
				    (the fixnum
				      (char-code (char string
							(the fixnum
							  (+ sidx n 1)))))))))
	    (incf chars))
	  (when (zerop n)
	    (setf value (the fixnum (ash value 8)))))
	(setf (schar result (the fixnum (+ didx 3)))
	      (if (> chars 3)
		  (schar encode-table (logand value #x3F))
		  pad))
	(setf value (the fixnum (ash value -6)))
	(setf (schar result (the fixnum (+ didx 2)))
	      (if (> chars 2)
		  (schar encode-table (logand value #x3F))
		  pad))
	(setf value (the fixnum (ash value -6)))
	(setf (schar result (the fixnum (1+ didx)))
	      (schar encode-table (logand value #x3F)))
	(setf value (the fixnum (ash value -6)))
	(setf (schar result didx)
	      (schar encode-table (logand value #x3F))))
      result)))


(defun round-next-multiple (x n)
  "Round x up to the next highest multiple of n"
  (declare (fixnum n)
	   (optimize (speed 3)))
  (let ((remainder (mod x n)))
    (declare (fixnum remainder))
    (if (zerop remainder)
	x
	(the fixnum (+ x (the fixnum (- n remainder)))))))

(defun integer-to-base64 (input &key (uri nil))
  "Encode an integer to base64 format."
  (declare (integer input)
	   (optimize (speed 3)))
  (let ((pad (if uri *uri-pad-char* *pad-char*))
	(encode-table (if uri *uri-encode-table* *encode-table*)))
    (declare (simple-string encode-table)
	     (character pad))
    (do* ((input-bits (integer-length input))
	  (byte-bits (round-next-multiple input-bits 8))
	  (padded-bits (round-next-multiple byte-bits 6))
	  (remainder-padding (mod padded-bits 24))
	  (padding-bits (if (zerop remainder-padding)
			    0
			    (- 24 remainder-padding)))
	  (strlen (/ (+ padded-bits padding-bits) 6))
	  (padding-chars (/ padding-bits 6))
	  (nonpad-chars (- strlen padding-chars))
	  (last-nonpad-char (1- nonpad-chars))
	  (str (make-string strlen))
	  (strpos 0 (1+ strpos))
	  (int (ash input (/ padding-bits 3)) (ash int -6))
	  (6bit-value (logand int #x3f) (logand int #x3f)))
	 ((= strpos nonpad-chars)
	  (dotimes (ipad padding-chars)
	    (setf (schar str strpos) pad)
	    (incf strpos))
	  str)
      (declare (fixnum 6bit-value strpos strlen last-nonpad-char)
	       (integer int))
      (setf (schar str (the fixnum (- last-nonpad-char strpos)))
	    (schar encode-table 6bit-value)))))

;;; Decoding

(defun base64-to-string (string &key (uri nil))
  "Decode a base64 string to a string array."
  (declare (string string)
	   (optimize (speed 3)))
  (let ((pad (if uri *uri-pad-char* *pad-char*))
	(decode-table (if uri *uri-decode-table* *decode-table*)))
    (declare (type decode-table decode-table)
	     (character pad))
    (let ((result (make-string (* 3 (truncate (/ (length string) 4)))))
	  (ridx 0))
      (declare (simple-string result)
	       (fixnum ridx))
      (loop
	 for char of-type character across string
	 for svalue of-type fixnum = (aref decode-table (the fixnum (char-code char)))
	 with bitstore of-type fixnum = 0
	 with bitcount of-type fixnum = 0
	 do
	   (cond
	     ((char= char pad)
	      ;; Could add checks to make sure padding is correct
	      ;; Currently, padding is ignored
	      )
	     ((minusp svalue)
	      (warn "Bad character ~W in base64 decode" char))
	     (t
	      (setf bitstore (logior
			      (the fixnum (ash bitstore 6))
			      svalue))
	      (incf bitcount 6)
	      (when (>= bitcount 8)
		(decf bitcount 8)
		(setf (char result ridx)
		      (code-char (the fixnum
				   (logand
				    (the fixnum
				      (ash bitstore
					   (the fixnum (- bitcount))))
				    #xFF))))
		(incf ridx)
		(setf bitstore (the fixnum (logand bitstore #xFF)))))))
      (subseq result 0 ridx))))
  
  
(defun base64-to-integer (string &key (uri nil))
  "Decodes a base64 string to an integer"
  (declare (string string)
	   (optimize (speed 3)))
  (let ((pad (if uri *uri-pad-char* *pad-char*))
	(decode-table (if uri *uri-decode-table* *decode-table*)))
    (declare (type decode-table decode-table)
	     (character pad))
    (let ((value 0))
      (declare (integer value))
      (loop
	 for char of-type character across string
	 for svalue of-type fixnum =
	   (aref decode-table (the fixnum (char-code char)))
	 do
	   (cond
	     ((char= char pad)
	      (setq value (the fixnum (ash value -2))))
	     ((minusp svalue)
	      (warn "Bad character ~W in base64 decode" char))
	     (t
	      (setq value (the fixnum
			    (+ svalue (the fixnum (ash value 6))))))))
      value)))
