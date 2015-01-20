#|
 This file is a part of South
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.south)

(defconstant +unix-epoch-difference+  (encode-universal-time 0 0 0 1 1 1970 0) "The universal to unix time difference in seconds.")
(defvar *external-format* :utf-8 "The external format used for encoding/decoding.")

(defun get-unix-time ()
  "Return the unix timestamp for GMT, as required by OAuth."
  (- (get-universal-time) +unix-epoch-difference+))

(defun generate-nonce ()
  "Generate a NONCE to use for requests. Currently this simply uses a v4-UUID."
  (write-to-string (uuid:make-v4-uuid)))

(defun parse-boolean (value)
  "Parses a string boolean. If the string is one of 
 (T, true, 1), then T is returned, otherwise NIL.
The check is case-insensitive."
  (when (or (string-equal value "true")
            (string-equal value "t")
            (string= value "1"))
    T))

(defun to-keyword (string)
  "Turns a key into a keyword.
Replaces _ with - and uppercases the string, then interns it
into the keyword package. This is useful to parse the request
responses into an alist."
  (let ((name (cl-ppcre:regex-replace-all "_" (string-upcase string) "-")))
    (or (find-symbol name "KEYWORD") (intern name "KEYWORD"))))

(defun url-encode (thing &optional (external-format *external-format*))
  "Returns a URL-encoded version of the string STRING or OCTET-SEQUENCE using the external format EXTERNAL-FORMAT.

According to spec https://dev.twitter.com/docs/auth/percent-encoding-parameters"
  ;; Adapted from DRAKMA.
  (with-output-to-string (out)
    (loop for octet across (etypecase thing
                             ((or string null)
                              (flexi-streams:string-to-octets (or thing "") :external-format external-format))
                             ((array (unsigned-byte 8))
                              thing))
          for char = (code-char octet)
          do (cond ((or (char<= #\0 char #\9)
                        (char<= #\a char #\z)
                        (char<= #\A char #\Z)
                        (find char "-._~" :test #'char=))
                    (write-char char out))
                   (t (format out "%~2,'0x" (char-code char)))))))

(defun url-decode (string &optional (external-format *external-format*))
  "Returns a URL-decoded version of the string STRING external format EXTERNAL-FORMAT.

According to spec https://dev.twitter.com/docs/auth/percent-encoding-parameters"
  (let ((out (make-array (length string) :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (loop for i from 0 below (length string)
          for char = (aref string i)
          do (case char
               (#\% (vector-push (parse-integer string :start (+ i 1) :end (+ i 3) :radix 16) out)
                    (incf i 2))
               (#\+ (vector-push (char-code #\Space) out))
               (T (vector-push (char-code char) out)))
          finally (return (flexi-streams:octets-to-string out :external-format external-format)))))

(defun hmac (string keystring)
  "Returns a base-64 encoded string of the HMAC digest of the given STRING
using the KEYSTRING as HMAC key. The encoding of *external-format* is used 
throughout."
  (let ((hmac (ironclad:make-hmac (flexi-streams:string-to-octets keystring :external-format *external-format*) :SHA1)))
    (ironclad:update-hmac hmac (flexi-streams:string-to-octets string :external-format *external-format*))
    (base64:usb8-array-to-base64-string
     (ironclad:hmac-digest hmac))))
