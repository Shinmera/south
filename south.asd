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
  :version "0.2.4"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Simple OaUTH library for oAuth1.0"
  :homepage "https://github.com/Shinmera/south"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "oauth"))
  :depends-on (:drakma
               :uuid
               :ironclad
               :cl-ppcre))
