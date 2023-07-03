(defsystem south
  :name "South"
  :version "0.3.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Simple OaUTH library for oAuth1.0"
  :homepage "https://Shinmera.github.io/south/"
  :bug-tracker "https://github.com/Shinmera/south/issues"
  :source-control (:git "https://github.com/Shinmera/south.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "oauth"))
  :depends-on (:drakma
               :uuid
               :ironclad
               :cl-ppcre))
