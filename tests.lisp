;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Tests for html-entities

(defpackage #:html-entities-tests
  (:use :cl :fiveam :html-entities))

(in-package :html-entities-tests)


(def-suite all
    :description "All html-entities tests")

(in-suite all)

(test conversion-hashes
  (let ((fiveam::*num-trials* 500))
      (for-all ((char (gen-character :code-limit 8000)))
        (let ((name (name-of-char char)))
          (if name
              (is (char= char (char-of-name name)))
              (is (null (char-of-name name))))))))

(test conversion-functions
  (let ((fiveam::*num-trials* 500))
    (for-all ((char (gen-character :code-limit 4000)))
      (is (char= char (entity-to-char (char-to-entity char))))
      (is (char= char (entity-to-char (format nil "&#~D;" (char-code char))))))))

(test string-conversion-functions
  (let ((fiveam::*num-trials* 100))
    (for-all ((str (gen-string :elements(gen-character :code-limit 100)))
              (*encode-using-named-entities* (gen-one-element t nil))
              (*encode-in-hexadecimal* (gen-one-element t nil)))
      (is (string= str (decode-entities (encode-entities str)))))
    (for-all ((str (gen-string :elements(gen-character :code-limit 1000)))
              (*encode-using-named-entities* (gen-one-element t nil))
              (*encode-in-hexadecimal* (gen-one-element t nil))
              (*enable-sgml* (gen-one-element t nil)))
      (is (string= str (decode-entities (encode-entities str)))))
    (for-all ((str (gen-string :elements(gen-character :code-limit 8000)))
              (*encode-using-named-entities* (gen-one-element t nil))
              (*encode-in-hexadecimal* (gen-one-element t nil))
              (*enable-sgml* (gen-one-element t nil)))
      (is (string= str (decode-entities (encode-entities str)))))
    ;; double-encoding and decoding
    (for-all ((str (gen-string :elements(gen-character :code-limit 100)))
              (*encode-using-named-entities* (gen-one-element t nil))
              (*encode-in-hexadecimal* (gen-one-element t nil)))
      (is (string= str (decode-entities
                        (decode-entities
                         (encode-entities
                          (encode-entities str)))))))))
