;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Main code for html-entities
;;; Inspired by Perl's HTML::Entities module

(in-package :html-entities)

;; one-time code, just to make the *entity-definitions* data structure

(defun slurp-text-file (filename)
  (with-open-file (f filename :direction :input)
    (let ((seq (make-array (file-length f) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq f))
      seq)))

(defun parse-entity-file (filename)
  (let ((text (regex-replace-all "(?s:<!--.*?-->)" (slurp-text-file filename) ""))
        (entities nil))
    (do-register-groups (name charcode) ("<!ENTITY\\s+(\\w+)\\s+\"&#(\\d+);\"\\s*>" text)
      (push (cons name (parse-integer charcode)) entities))
    (nreverse entities)))

(defun parse-sgml-ent-file (filename)
  (let ((text (slurp-text-file filename))
        (entities nil))
    (do-register-groups (name unicode) ("(?m:^).\\s+(\\S+)\\s+\\S+\\s+([0-9A-Fa-f]+)\\s+" text)
      (push (cons name (parse-integer unicode :radix 16)) entities))
    (nreverse entities)))

(defparameter *entity-files*
  '("xhtml-special.ent"
    "xhtml-lat1.ent"
    "xhtml-symbol.ent"))

(defparameter *sgml-entity-files*
  '("sgml-entities.txt"))

(defun pretty-print-alist (x &optional stream)
  (princ "(" stream)
  (terpri stream)
  (loop for sublist in x do
        (format (or stream t) " (~10@<~S~> . ~6<#x~X~>) ; ~:*~D = ~S~%"
                (car sublist)
                (cdr sublist)
                (code-char (cdr sublist))))
  (princ ")" stream)
  t)


;; there's still some hand-editing after running this function
(defun generate-entity-definitions (outfile)
  (let ((html-ents (loop for file in *entity-files*
                           appending (parse-entity-file file)))
        (sgml-ents (loop for file in *sgml-entity-files*
                         appending (parse-sgml-ent-file file))))
    (with-open-file (F outfile :direction :output :if-exists :supersede)
      (format F ";;; HTML Entity definitions~%~%")
      (pretty-print-alist (sort html-ents #'< :key #'cdr) F)
      (terpri F)
      (terpri F)
      (format F ";;; SGML Entity definitions~%~%")
      (pretty-print-alist (sort sgml-ents #'< :key #'cdr) F)
      (terpri F))))
