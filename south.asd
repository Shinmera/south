#|
  This file is a part of South
  (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defsystem south
  :name "South"
  :version "0.3.0"
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
