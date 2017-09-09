;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Main code for html-entities
;;; Inspired by Perl's HTML::Entities module
;;; See entity-tables.lisp for the lookup tables


(in-package :html-entities)


;; HTML conversions
(defvar *name-to-char*      (make-hash-table :test 'equal))
(defvar *char-to-name*      (make-hash-table :test 'eq))

;; SGML conversions
(defvar *sgml-name-to-char* (make-hash-table :test 'equal))
(defvar *sgml-char-to-name* (make-hash-table :test 'eq))


(defun init-hashes ()
  (loop for (name . code) in *sgml-entity-definitions*
        do (let ((char (code-char code)))
             (setf (gethash name *sgml-name-to-char*) char)
             (setf (gethash char *sgml-char-to-name*) name)))
  (loop for (name . code) in *entity-definitions*
        do (let ((char (code-char code)))
             (setf (gethash name *name-to-char*) char)
             (setf (gethash char *char-to-name*) name))))

(defvar *encode-using-named-entities* t
  "Use symbolic names, such as  &aacute; , rather than
 &#225;  or  &#xE1; .")

(defvar *encode-in-hexadecimal* t
  "When a symbolic name is not available, use hexadecimal-based
integers: &#xE1; instead of &#225; .")

(defvar *enable-sgml* nil
  "Encode and decode using SGML entity names, of which HTML entity names
are almost a subset (&apos; is different).")

(defun name-of-char (char)
  (gethash char (if *enable-sgml*
                    *sgml-char-to-name*
                    *char-to-name*)))

(defun char-of-name (name)
  (gethash name (if *enable-sgml*
                    *sgml-name-to-char*
                    *name-to-char*)))


(defun char-to-entity (char-or-string)
  (let* ((c (if (stringp char-or-string)
                (char char-or-string 0)
                char-or-string))
         (name (when *encode-using-named-entities*
                 (name-of-char c))))
    (assert (characterp c))
    (if (and name *encode-using-named-entities*)
        (format nil "&~A;" name)
        (format nil (if *encode-in-hexadecimal* "&#x~X;" "&#~D;") (char-code c)))))

(defun entity-to-char (string)
  (assert (char= (char string 0) #\&))
  (let* ((end (1- (length string)))
         (meat (subseq string
                       1
                       (if (char= (char string end) #\;)
                           end
                           (1+ end)))))
    (let ((c (char-of-name meat)))
      (cond (c
             c)
            ((string-equal (subseq meat 0 2) "#x")
             (code-char (parse-integer (subseq meat 2) :radix 16)))
            ((string= (subseq meat 0 1) "#")
             (code-char (parse-integer (subseq meat 1))))
            (t (error "Unknown or invalid entity ~A" string))))))

(defparameter *escape-all* "[^\n\r\t !\#\$%\(-;=?-~]")

(defun encode-entities (html &key (regex *escape-all*))
  "Encodes entites in HTML.  Defaults to pretty much everything you can, but
you can pass a regex like \"[<>&\\\"]\" to only encode the basics."
  (regex-replace-all regex html #'char-to-entity :simple-calls t))

(defun decode-entities (html)
  "Decodes all entites in HTML."
  (regex-replace-all "&(?:[\\w\\.]+|#[xX][a-zA-Z0-9]+|#\\d+);?" html
                     (lambda (x) (format nil "~C" (entity-to-char x))) :simple-calls t))


(init-hashes)

