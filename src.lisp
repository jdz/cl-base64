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
;;;; $Id: src.lisp,v 1.3 2003/01/04 06:13:53 kevin Exp $

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


;;; Utilities

(defun round-next-multiple (x n)
  "Round x up to the next highest multiple of n."
  (declare (fixnum n)
	   (optimize (speed 3)))
  (let ((remainder (mod x n)))
    (declare (fixnum remainder))
    (if (zerop remainder)
	x
	(the fixnum (+ x (the fixnum (- n remainder)))))))

(declaim (inline whitespace-p))
(defun whitespace-p (c)
  "Returns T for a whitespace character."
  (or (char= c #\Newline) (char= c #\Linefeed)
      (char= c #\Return) (char= c #\Space)
      (char= c #\Tab)))


;; Encode routines

(defun string-to-base64 (string &key (uri nil) (columns 0) (stream nil))
  "Encode a string array to base64. If columns is > 0, designates
maximum number of columns in a line and the string will be terminated
with a #\Newline."
  (declare (string string)
	   (fixnum columns)
	   (optimize (speed 3)))
  (let ((pad (if uri *uri-pad-char* *pad-char*))
	(encode-table (if uri *uri-encode-table* *encode-table*)))
    (declare (simple-string encode-table)
	     (character pad))
    (let* ((string-length (length string))
	   (complete-group-count (truncate string-length 3))
	   (remainder (nth-value 1 (truncate string-length 3)))
	   (padded-length (+ remainder
			     (* 4 complete-group-count)))
	   (num-lines (if (plusp columns)
			  (truncate (+ padded-length (1- columns)) columns)
			  0))
	   (num-breaks (if (plusp num-lines)
			   (1- num-lines)
			   0))
	   (strlen (+ padded-length num-breaks))
	   (result (unless stream
		     (make-string strlen)))
	   (col (if (plusp columns)
		    0
		    (1+ padded-length)))
	   (ioutput 0))
      (declare (fixnum string-length padded-length col ioutput)
	       (simple-string result))
      (labels ((output-char (ch)
		 (when (= col columns)
		   (if stream
		       (write #\Newline stream)
		       (progn
			 (setf (schar result ioutput) #\Newline)
			 (incf ioutput)))
		   (setq col 0))
		 (incf col)
		 (if stream
		     (write ch stream)
		     (progn
		       (setf (schar result ioutput) ch)
		       (incf ioutput))))
	     (output-group (svalue chars)
	       (output-char
		(schar encode-table
		       (the fixnum
			 (logand #x3f
				 (the fixnum (ash svalue -18))))))
	       (output-char
		(schar encode-table
		       (the fixnum
			 (logand #x3f
				 (the fixnum (ash svalue -12))))))
	       (if (> chars 2)
		   (output-char
		    (schar encode-table
			   (the fixnum
			     (logand #x3f
				     (the fixnum (ash svalue -6))))))
		   (output-char pad))
	       (if (> chars 3)
		   (output-char
		    (schar encode-table
			   (the fixnum
			     (logand #x3f svalue))))
		   (output-char pad))))
	(do ((igroup 0 (1+ igroup))
	     (isource 0 (+ isource 3))
	     svalue)
	    ((= igroup complete-group-count)
	     (case remainder
	       (2
		(setq svalue
		      (the fixnum
			(+
			 (the fixnum
			   (ash (char-code (the character
					     (char string isource))) 16))
			 (the fixnum
			   (ash (char-code (the character
					     (char string (1+ isource)))) 8)))))
		(output-group svalue 3))
	       (1
		(setq svalue
		      (the fixnum
			(char-code (the character
				     (char string isource)))))
		(output-group svalue 2)))
	     result)
	  (declare (fixnum igroup isource svalue))
	  (setq svalue
		(the fixnum
		  (+
		   (the fixnum
		     (ash (char-code (the character
				       (char string isource))) 16))
		   (the fixnum
		     (ash (char-code (the character
				       (char string (1+ isource)))) 8))
		   (the fixnum
		     (char-code (the character
				  (char string (+ 2 isource))))))))
	  (output-group svalue 4))))))
  
  
(defun integer-to-base64 (input &key (uri nil) (columns 0) (stream nil))
  (if stream
      (integer-to-base64-stream input stream :uri uri :columns columns)
      (integer-to-base64-string input :uri uri :columns columns)))

(defun integer-to-base64-string (input &key (uri nil) (columns 0))
  "Encode an integer to base64 format."
  (declare (integer input)
	   (fixnum columns)
	   (optimize (speed 3)))
  (let ((pad (if uri *uri-pad-char* *pad-char*))
	(encode-table (if uri *uri-encode-table* *encode-table*)))
    (declare (simple-string encode-table)
	     (character pad))
    (let* ((input-bits (integer-length input))
	   (byte-bits (round-next-multiple input-bits 8))
	   (padded-bits (round-next-multiple byte-bits 6))
	   (remainder-padding (mod padded-bits 24))
	   (padding-bits (if (zerop remainder-padding)
			     0
			     (- 24 remainder-padding)))
	   (padding-chars (/ padding-bits 6))
	   (padded-length (/ (+ padded-bits padding-bits) 6))
	   (last-line-len (if (plusp columns)
			      (- padded-length (* columns
						  (truncate
						   padded-length columns)))
			      0))
	   (num-lines (if (plusp columns)
			  (truncate (+ padded-length (1- columns)) columns)
			  0))
	   (num-breaks (if (plusp num-lines)
			   (1- num-lines)
			   0))
	   (strlen (+ padded-length num-breaks))
	   (last-char (1- strlen))
	   (str (make-string strlen))
	   (col (if (zerop last-line-len)
		    (1- columns)
		    (1- last-line-len))))
      (declare (fixnum padded-length num-lines col last-char
		       padding-chars last-line-len))
      (unless (plusp columns)
	(setq col -1)) ;; set to flag to optimize in loop
      
      (dotimes (i padding-chars)
	(declare (fixnum i))
	(setf (schar str (the fixnum (- last-char i))) pad))

      (do* ((strpos (- last-char padding-chars) (1- strpos))
	    (int (ash input (/ padding-bits 3))))
	   ((minusp strpos)
	    str)
	(declare (fixnum strpos) (integer int))
	(cond
	  ((zerop col)
	   (setf (schar str strpos) #\Newline)
	   (setq col columns))
	  (t
	   (setf (schar str strpos)
		 (schar encode-table (the fixnum (logand int #x3f))))
	   (setq int (ash int -6))
	   (decf col)))))))

(defun integer-to-base64-stream (input stream &key (uri nil) (columns 0))
  "Encode an integer to base64 format."
  (declare (integer input)
	   (fixnum columns)
	   (optimize (speed 3)))
  (let ((pad (if uri *uri-pad-char* *pad-char*))
	(encode-table (if uri *uri-encode-table* *encode-table*)))
    (declare (simple-string encode-table)
	     (character pad))
    (let* ((input-bits (integer-length input))
	   (byte-bits (round-next-multiple input-bits 8))
	   (padded-bits (round-next-multiple byte-bits 6))
	   (remainder-padding (mod padded-bits 24))
	   (padding-bits (if (zerop remainder-padding)
			     0
			     (- 24 remainder-padding)))
	   (padding-chars (/ padding-bits 6))
	   (padded-length (/ (+ padded-bits padding-bits) 6))
	   (strlen padded-length)
	   (nonpad-chars (- strlen padding-chars))
	   (last-nonpad-char (1- nonpad-chars))
	   (str (make-string strlen)))
      (declare (fixnum padded-length last-nonpad-char))
      (do* ((strpos 0 (1+ strpos))
	    (int (ash input (/ padding-bits 3)) (ash int -6))
	    (6bit-value (logand int #x3f) (logand int #x3f)))
	   ((= strpos nonpad-chars)
	    (let ((col 0))
	      (declare (fixnum col))
	      (dotimes (i nonpad-chars)
		(declare (fixnum i))
		(write-char (schar str i) stream)
		(when (plusp columns)
		  (incf col)
		  (when (= col columns)
		    (write-char #\Newline stream)
		    (setq col 0))))
	      (dotimes (ipad padding-chars)
		(declare (fixnum ipad))
		(write-char pad stream)
		(when (plusp columns)
		  (incf col)
		  (when (= col columns)
		    (write-char #\Newline stream)
		    (setq col 0)))))
	    stream)
	(declare (fixnum 6bit-value strpos)
		 (integer int))
	(setf (schar str (- last-nonpad-char strpos))
	      (schar encode-table 6bit-value))
	))))

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
	 for svalue of-type fixnum = (aref decode-table (the fixnum (char-code char)))
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
