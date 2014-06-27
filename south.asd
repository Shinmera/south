#|
  This file is a part of South
  (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.south.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.south.asdf)

(defsystem south
  :name "South"
  :version "0.2.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Simple OaUTH library for oAuth1.0"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "oauth"))
  :depends-on (:drakma
               :yason
               :uuid
               :ironclad
               :cl-ppcre))

;; (defsystem south-doc
;;   :name "South Doc"
;;   :components ((:file "documentation"))
;;   :depends-on (:colleen :lquery-doc))
