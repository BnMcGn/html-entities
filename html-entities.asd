;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(defpackage #:html-entities-asd
  (:use :cl :asdf))

(in-package :html-entities-asd)


(defsystem html-entities
  :name "html-entities"
  :description "A module for encoding and decoding HTML/XML/SGML entities."
  :version "0.02"
  :author "Aaron Sokoloski <asokoloski@gmail.com>"
  :licence "MIT License"
  :depends-on (:cl-ppcre)
  :components ((:file "packages")
               (:file "entity-tables" :depends-on ("packages"))
               (:file "html-entities" :depends-on ("entity-tables" "packages")))

  :in-order-to ((test-op (load-op html-entities-tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :fiveam)
                             (intern "ALL"
                                     :html-entities-tests)))
)


(defsystem html-entities-tests
  :components ((:file "tests"))
  :depends-on (:html-entities :fiveam))



;; all test systems
(defmethod operation-done-p ((o test-op) (c system))
  (values nil))

(defmethod operation-done-p ((o test-op)
                             (c (eql (find-system 'html-entities-tests))))
  (values nil))

