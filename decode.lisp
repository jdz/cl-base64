;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          encode.lisp
;;;; Purpose:       cl-base64 encoding routines
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Dec 2002
;;;;
;;;; $Id$
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

(in-package #:cl-base64)

(declaim (inline whitespace-p))
(defun whitespace-p (c)
  "Returns T for a whitespace character."
  (or (char= c #\Newline) (char= c #\Linefeed)
      (char= c #\Return) (char= c #\Space)
      (char= c #\Tab)))

;;; Decoding

(defmacro def-base64-string-to-* (output-type)
  `(defun ,(intern (concatenate 'string (symbol-name '#:base64-string-to-)
                                (symbol-name output-type)))
       (input &key (uri nil)
                   ,@(when (eq output-type :stream)
                       '(stream)))
     ,(concatenate 'string "Decode base64 string to "
                   (string-downcase (symbol-name output-type)))
     (declare (string input)
              (optimize (speed 3) (safety 1) (space 0)))
     (let ((pad (if uri *uri-pad-char* *pad-char*))
           (decode-table (if uri *uri-decode-table* *decode-table*)))
       (declare (type decode-table decode-table)
                (type character pad))
       (let (,@(case output-type
                 (:string
                  '((result (make-string (* 3 (truncate (length input) 4))))))
                 (:usb8-array
                  '((result (make-array (* 3 (truncate (length input) 4))
                             :element-type '(unsigned-byte 8))))))
             (ridx 0))
         (declare ,@(case output-type
                      (:string
                       '((type simple-string result)))
                      (:usb8-array
                       '((type (simple-array (unsigned-byte 8) (*)) result))))
                  (fixnum ridx))
         (loop
           with bitstore of-type fixnum = 0
           with bitcount of-type fixnum = 0
           for char of-type character across input
           for svalue of-type fixnum = (aref decode-table
                                             (the fixnum (char-code char)))
           do (cond
                ((>= svalue 0)
                 (setf bitstore (logior
                                 (the fixnum (ash bitstore 6))
                                 svalue))
                 (incf bitcount 6)
                 (when (>= bitcount 8)
                   (decf bitcount 8)
                   (let ((ovalue (the fixnum
                                      (logand
                                       (the fixnum
                                            (ash bitstore
                                                 (the fixnum (- bitcount))))
                                       #xFF))))
                     (declare (fixnum ovalue))
                     ,(ecase output-type
                        (:string
                         '(setf (char result ridx) (code-char ovalue)))
                        (:usb8-array
                         '(setf (aref result ridx) ovalue))
                        (:stream
                         '(write-char (code-char ovalue) stream)))
                     (incf ridx)
                     (setf bitstore (the fixnum (logand bitstore #xFF))))))
                ((char= char pad)
                 ;; Could add checks to make sure padding is correct
                 ;; Currently, padding is ignored
                 )
                ((whitespace-p char)
                 ;; Ignore whitespace
                 )
                ((minusp svalue)
                 (error "Bad character ~W in base64 decode" char))))
         ,(case output-type
            (:stream
             'stream)
            ((:usb8-array :string)
             '(subseq result 0 ridx)))))))

(def-base64-string-to-* :string)
(def-base64-string-to-* :stream)
(def-base64-string-to-* :usb8-array)

;; input-mode can be :string or :stream
;; input-format can be :character or :usb8

(defun base64-string-to-integer (string &key (uri nil))
  "Decodes a base64 string to an integer"
  (declare (string string)
           (optimize (speed 3) (safety 1) (space 0)))
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
           (optimize (speed 3) (space 0) (safety 1)))
  (let ((pad (if uri *uri-pad-char* *pad-char*))
        (decode-table (if uri *uri-decode-table* *decode-table*)))
    (declare (type decode-table decode-table)
             (character pad))
    (do* ((value 0)
          (char (read-char stream nil #\null)
                (read-char stream nil #\null)))
         ((eq char #\null)
          value)
      (declare (integer value)
               (character char))
      (let ((svalue (aref decode-table (the fixnum (char-code char)))))
           (declare (fixnum svalue))
           (cond
             ((>= svalue 0)
              (setq value (+ svalue (ash value 6))))
             ((char= char pad)
              (setq value (ash value -2)))
             ((whitespace-p char)               ; ignore whitespace
              )
             ((minusp svalue)
              (warn "Bad character ~W in base64 decode" char)))))))
