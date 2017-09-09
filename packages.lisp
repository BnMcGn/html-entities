;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(defpackage #:html-entities
  (:use :cl :ppcre)
  (:documentation "Main package for html-entities.")
  (:export
   #:char-of-name
   #:name-of-char
   #:char-to-entity
   #:entity-to-char
   #:encode-entities
   #:decode-entities
   #:*encode-using-named-entities*
   #:*encode-in-hexadecimal*
   #:*enable-sgml*
))


