;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          encode.lisp
;;;; Purpose:       cl-base64 encoding routines
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Dec 2002
;;;;
;;;; $Id: decode.lisp,v 1.1 2003/01/12 20:25:26 kevin Exp $
;;;;
;;;; This file implements the Base64 transfer encoding algorithm as
;;;; defined in RFC 1521 by Borensten & Freed, September 1993.
;;;; See: http://www.ietf.org/rfc/rfc1521.txt
;;;;
;;;; Based on initial public domain code by Juri Pakaste <juri@iki.fi>
;;;;
;;;; Copyright 2002-2003 Kevin M. Rosenberg
;;;; Permission to use with BSD-style license included in the COPYING file
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))

(declaim (inline whitespace-p))
(defun whitespace-p (c)
  "Returns T for a whitespace character."
  (or (char= c #\Newline) (char= c #\Linefeed)
      (char= c #\Return) (char= c #\Space)
      (char= c #\Tab)))


;;; Decoding

(defun base64-to-string (string &key (uri nil))
  "Decode a base64 string to a string array."
  (declare (string string)
	   (optimize (speed 3)))
  (let ((pad (if uri *uri-pad-char* *pad-char*))
	(decode-table (if uri *uri-decode-table* *decode-table*)))
    (declare (type decode-table decode-table)
	     (character pad))
    (let ((result (make-string (* 3 (truncate (length string) 4))))
	  (ridx 0))
      (declare (simple-string result)
	       (fixnum ridx))
      (loop
	 for char of-type character across string
	 for svalue of-type fixnum = (aref decode-table
					   (the fixnum (char-code char)))
	 with bitstore of-type fixnum = 0
	 with bitcount of-type fixnum = 0
	 do
	   (cond
	     ((>= svalue 0)
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
		(setf bitstore (the fixnum (logand bitstore #xFF)))))
	     ((char= char pad)
	      ;; Could add checks to make sure padding is correct
	      ;; Currently, padding is ignored
	      )
	     ((whitespace-p char)
	      ;; Ignore whitespace
	      )
	     ((minusp svalue)
	      (warn "Bad character ~W in base64 decode" char))
))
      (subseq result 0 ridx))))

#|
(def-base64-stream-to-* :string)
(def-base64-stream-to-* :stream)
(def-base64-stream-to-* :usb8-array)
|#

(defmacro def-base64-string-to-* (output-type)
  `(defun ,(case output-type
	    (:string
	     'base64-string-to-string)
	    (:stream
	     'base64-string-to-stream)
	    (:usb8-array
	     'base64-string-to-usb8-array))
       (input &key (uri nil)
	,@(when (eq output-type :stream)
		'(stream)))
     "Decode base64 string"
     (declare (input string)
	      (optimize (speed 3)))
     (let ((pad (if uri *uri-pad-char* *pad-char*))
	   (decode-table (if uri *uri-decode-table* *decode-table*)))
       (declare (type decode-table decode-table)
		(character pad))
       (let (,@(case output-type
		     (:string
		      '((result (make-string (* 3 (truncate (length string) 4))))))
		     (:usb8-array
		      '((result (make-array (* 3 (truncate (length string) 4))
				 :element-type '(unsigned-byte 8)
				 :fill-pointer nil
				 :adjustable nil)))))
	       (ridx 0))
	 (declare ,@(case output-type
			  (:string
			   '((simple-string result))
			   (:usb8-array
			    '((type (array fixnum (*)) result)))))
		  (fixnum ridx))
	 (loop 
	    for char of-type character across string
	    for svalue of-type fixnum = (aref decode-table
					      (the fixnum (char-code char)))
	    with bitstore of-type fixnum = 0
	    with bitcount of-type fixnum = 0
	    do
	      (cond
		((>= svalue 0)
		 (setf bitstore (logior
				 (the fixnum (ash bitstore 6))
				 svalue))
		 (incf bitcount 6)
		 (when (>= bitcount 8)
		   (decf bitcount 8)
		   (let ((svalue (the fixnum
				   (logand
				    (the fixnum
				      (ash bitstore
					   (the fixnum (- bitcount))))
				    #xFF))))
		     (declare (fixnum svalue))
		     ,@(case output-type
			     (:string
			      (setf (char result ridx) (code-char svalue)))
			     (:usb8-array
			      (setf (aref result ridx) svalue))
			     (:stream
			      (write-char (code-char svalue) stream)))
		     (incf ridx)
		     (setf bitstore (the fixnum (logand bitstore #xFF)))))
		 ((char= char pad)
		  ;; Could add checks to make sure padding is correct
		  ;; Currently, padding is ignored
		  )
		 ((whitespace-p char)
		  ;; Ignore whitespace
		  )
		 ((minusp svalue)
		  (warn "Bad character ~W in base64 decode" char))
		 ))
	      (subseq result 0 ridx))))))

(def-base64-string-to-* :string)
(def-base64-string-to-* :stream)
(def-base64-string-to-* :usb8-array)
  
;; input-mode can be :string or :stream
;; input-format can be :character or :usb8

(defun base64-string-to-integer (string &key (uri nil))
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
	     ((>= svalue 0)
	      (setq value (+ svalue (ash value 6))))
	     ((char= char pad)
	      (setq value (ash value -2)))
	     ((whitespace-p char)
	      ; ignore whitespace
	      )
	     ((minusp svalue)
	      (warn "Bad character ~W in base64 decode" char))))
      value)))

(defun base64-stream-to-integer (stream &key (uri nil))
  "Decodes a base64 string to an integer"
  (declare (stream stream)
	   (optimize (speed 3)))
  (let ((pad (if uri *uri-pad-char* *pad-char*))
	(decode-table (if uri *uri-decode-table* *decode-table*)))
    (declare (type decode-table decode-table)
	     (character pad))
    (do* ((value 0)
	  (char (read-char stream nil #\null)
		(read-char stream nil #\null)))
	 ((eq char #\null)
	  value)
      (declare (value integer)
	       (char character))
      (let ((svalue (aref decode-table (the fixnum (char-code char)))))
	   (declare (fixnum svalue))
	   (cond
	     ((>= svalue 0)
	      (setq value (+ svalue (ash value 6))))
	     ((char= char pad)
	      (setq value (ash value -2)))
	     ((whitespace-p char)		; ignore whitespace
	      )
	     ((minusp svalue)
	      (warn "Bad character ~W in base64 decode" char))))
	value)))
