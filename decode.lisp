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

(define-condition base64-error (error)
  ((input
    :initarg :input
    :reader base64-error-input)
   (position
    :initarg :position
    :reader base64-error-position
    :type unsigned-byte)))

(define-condition bad-base64-character (base64-error)
  ((code :initarg :code :reader bad-base64-character-code))
  (:report (lambda (condition stream)
             (format stream "Bad character ~S at index ~D of ~S"
                     (code-char (bad-base64-character-code condition))
                     (base64-error-position condition)
                     (base64-error-input condition)))))

(define-condition incomplete-base64-data (base64-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Unexpected end of Base64 data at index ~D of ~S"
                     (base64-error-position condition)
                     (base64-error-input condition)))))

(deftype array-index (&optional (length array-dimension-limit))
  `(integer 0 (,length)))

(deftype array-length (&optional (length array-dimension-limit))
  `(integer 0 ,length))

(deftype character-code ()
  `(integer 0 (,char-code-limit)))

(defmacro etypecase/unroll ((var &rest types) &body body)
  `(etypecase ,var
     ,@(loop for type in types
             collect `(,type ,@body))))

(defmacro define-base64-decoder (hose sink)
  `(defun ,(intern (format nil "~A-~A-~A-~A" '#:base64 hose '#:to sink))
       (input &key (uri nil) ,@(when (eq sink :stream)
                                 '(stream)))
     ,(format nil "Decode Base64 ~(~A~) to ~(~A~)." hose sink)
     (declare (optimize (speed 3) (safety 1)))
     (let ((decode-table (if uri *uri-decode-table* *decode-table*))
           ,@(case sink
               (:usb8-array
                '((result (make-array (* 3 (ceiling (length input) 4))
                           :element-type '(unsigned-byte 8)))
                  (rpos 0)))
               (:string
                '((result (make-string (* 3 (ceiling (length input) 4))))
                  (rpos 0)))
               (:integer
                '((result 0)))))
       (declare (type decode-table decode-table)
                ,@(case sink
                    (:usb8-array
                     '((type (vector (unsigned-byte 8)) result)
                       (type array-index rpos)))
                    (:string
                     '((type simple-string result)
                       (type array-index rpos)))
                    (:integer
                     '((type unsigned-byte result)))))
       (flet ((bad-char (pos code)
                (error 'bad-base64-character :input input
                                             :position pos
                                             :code code))
              (incomplete-input (pos)
                (error 'incomplete-base64-data :input input :position pos)))
         (etypecase/unroll (input simple-base-string simple-string string)
           (let ((length (length input))
                 (ipos 0)
                 (code 0)
                 (bitstore 0)
                 (bitcount 0)
                 (svalue -1)
                 (padchar 0))
             (declare (type array-length length)
                      (type array-index ipos)
                      (type (unsigned-byte 24) bitstore)
                      (type (integer 0 14) bitcount)
                      (type (signed-byte 8) svalue)
                      (type (integer 0 3) padchar))
             (loop
               ,@(ecase hose
                   (:string
                    '((if (< ipos length)
                          (setq code (char-code (aref input ipos)))
                          (return)))))
                 (cond
                   ((or (< 127 code)
                        (= -1 (setq svalue (aref decode-table code))))
                    (bad-char ipos code))
                   ((= -2 svalue)
                    (cond ((<= (incf padchar) 2)
                           (unless (<= 2 bitcount)
                             (bad-char ipos code))
                           (decf bitcount 2))
                          (t
                           (bad-char ipos code))))
                   ((= -3 svalue)
                    ;; Ignore whitespace.
                    )
                   ((not (zerop padchar))
                    (bad-char ipos code))
                   (t
                    (setf bitstore (logior (ash bitstore 6) svalue))
                    (incf bitcount 6)
                    (when (>= bitcount 8)
                      (decf bitcount 8)
                      (let ((byte (ldb (byte 8 bitcount) bitstore)))
                        (declare (type (unsigned-byte 8) byte))
                        ,@(case sink
                            (:usb8-array
                             '((setf (aref result rpos) byte)
                               (incf rpos)))
                            (:string
                             '((setf (schar result rpos) (code-char byte))
                               (incf rpos)))
                            (:integer
                             '((setq result (logior (ash result 8) byte))))
                            (:stream
                             '((write-char (code-char byte) stream)))))
                      (setf bitstore (logand bitstore #xFF)))))
                 (incf ipos))
             (unless (zerop bitcount)
               (incomplete-input ipos))
             ,@(ecase sink
                 ((:string :usb8-array)
                  '((if (= rpos (length result))
                        result
                        (subseq result 0 rpos))))
                 (:integer
                  '(result))
                 (:stream
                  '(stream)))))))))

(define-base64-decoder :string :usb8-array)
(define-base64-decoder :string :string)
(define-base64-decoder :string :integer)
(define-base64-decoder :string :stream)

;; input-mode can be :string or :stream
;; input-format can be :character or :usb8

(defun base64-stream-to-integer (stream &key (uri nil))
  "Decodes a base64 stream to an integer."
  (declare (stream stream)
           (optimize (speed 3) (space 0) (safety 1)))
  (let ((decode-table (if uri *uri-decode-table* *decode-table*)))
    (declare (type decode-table decode-table))
    (loop with value of-type integer = 0
          with svalue of-type (signed-byte 8) = 0
          for char of-type (or null character) = (read-char stream nil nil)
          for code of-type fixnum = (char-code char)
          do (cond
               ((or (< 255 code)
                    (= -1 (setq svalue (aref decode-table code))))
                (error "Bad character ~W in base64 decode" char))
               ((= -2 svalue)
                ;; TODO: Add checks to make sure padding is correct.
                (setq value (ash value -2)))
               ((= -3 svalue)
                ;; Ignore whitespace.
                )
               (t
                (setq value (+ svalue (ash value 6))))))))
