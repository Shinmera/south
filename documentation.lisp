#|
 This file is a part of South
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.tymoonnext.south.doc
  (:use #:cl #:lquery #:lquery-doc)
  (:nicknames #:south-doc)
  (:export #:build-documentation))

(in-package #:org.tymoonnext.south.doc)

(defmethod documentate-object :after (template object fields)
  ($ template ".anchor" (attr :name (symbol-name (nth 0 object))))
  ($ template "h3 a" (attr :href (format NIL "#~a" (symbol-name (nth 0 object))))))

(defun build-documentation ()
  ($ (initialize (merge-pathnames "about-template.html" (asdf:system-source-directory :south))))
  (let ((template ($ "#template")))
    (let ((nodes (lquery-doc::documentate template :south :exclude '(:internal :method))))
      ($ "#docs" (empty) (append nodes))))
  ($ (write-to-file (merge-pathnames "about.html" (asdf:system-source-directory :south)))))

