#|
 This file is a part of South
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.south)

(defvar *oauth-api-key* NIL)
(defvar *oauth-api-secret* NIL)
(defvar *oauth-access-token* NIL)
(defvar *oauth-access-secret* NIL)
(defvar *oauth-signature-method* "HMAC-SHA1")
(defvar *oauth-version* "1.0")
(defvar *oauth/request-token* NIL)
(defvar *oauth/authenticate* NIL)
(defvar *oauth/authorize* NIL)
(defvar *oauth/access-token* NIL)
(defvar *server-port* 8989)
(defvar *authentication-callback* #'(lambda (&rest a) (declare (ignore a)))
  "Function called when COMPLETE-AUTHENTICATION is called.
Two arguments are passed, the ACCESS-TOKEN and the ACCESS-SECRET.")

(defun reset ()
  "Resets the internal API-KEY, API-SECRET, ACCESS-TOKEN and ACCESS-SECRET to NIL.
This does not change the oauth request URLs."
  (setf *oauth-api-key* NIL
        *oauth-api-secret* NIL
        *oauth-access-token* NIL
        *oauth-access-secret* NIL))

(defun prepare (&key oauth/request-token oauth/authorize (oauth/authenticate oauth/authorize) oauth/access-token api-key api-secret)
  "Cosmetic function to set the oauth request URLs and api-key and -secret.
This only sets the internal value of the parameter if it is non-NIL."
  (when oauth/request-token (setf *oauth/request-token* oauth/request-token))
  (when oauth/authorize (setf *oauth/authorize* oauth/authorize))
  (when oauth/authenticate (setf *oauth/authenticate* oauth/authenticate))
  (when oauth/access-token (setf *oauth/access-token* oauth/access-token))
  (when api-key (setf *oauth-api-key* api-key))
  (when api-secret (setf *oauth-api-secret* api-secret)))

(defmacro with-oauth-environment ((&key oauth/request-token oauth/authorize (oauth/authenticate oauth/authorize) oauth/access-token
                                     signature-method version
                                     api-key api-secret access-token access-secret) &body body)
  "Establishes an environment for all oauth related special variables.
Unless a non-NIL value is provided for a parameter it is bound to the current value of the according special variable."
  `(let ((*oauth-api-key* (or ,api-key *oauth-api-key*))
         (*oauth-api-secret* (or ,api-secret *oauth-api-secret*))
         (*oauth-access-token* (or ,access-token *oauth-access-token*))
         (*oauth-access-secret* (or ,access-secret *oauth-access-secret*))
         (*oauth-signature-method* (or ,signature-method *oauth-signature-method*))
         (*oauth-version* (or ,version *oauth-version*))
         (*oauth/request-token* (or ,oauth/request-token *oauth/request-token*))
         (*oauth/authenticate* (or ,oauth/authenticate *oauth/authenticate*))
         (*oauth/authorize* (or ,oauth/authorize *oauth/authorize*))
         (*oauth/access-token* (or ,oauth/access-token *oauth/access-token*)))
     ,@body))

(define-condition oauth-error (error) ())

(define-condition oauth-parameter-missing (oauth-error)
  ((%parameter :initarg :parameter :accessor parameter))
  (:report (lambda (c s) (format s "The ~a parameter is required for OAuth, but is not set." (parameter c)))))

(define-condition oauth-request-error (oauth-error)
  ((%http-status :initarg :status :accessor http-status)
   (%http-body :initarg :body :accessor http-body)
   (%http-headers :initarg :headers :accessor http-headers)
   (%target-url :initarg :url :accessor target-url)
   (%target-method :initarg :method :accessor target-method)
   (%target-parameters :initarg :parameters :accessor target-parameters)
   (%target-headers :initarg :sent-headers :accessor target-headers))
  (:report (lambda (c s) (format s "OAuth Request Failed: Status code ~d when requesting ~a :~%~s"
                                 (http-status c) (target-url c) (http-body c)))))

(defun oauth-response->alist (body &optional spec)
  "Turn an oauth-response into an ALIST."
  (mapcar #'(lambda (assignment)
              (let* ((pair (cl-ppcre:split "=" assignment))
                     (key (to-keyword (first pair)))
                     (val (second pair))
                     (parser (cdr (assoc key spec))))
                (cons key (if parser (funcall parser val) val))))
          (cl-ppcre:split "&" body)))

(defun signature-format-parameter (s param &rest rest)
  (declare (ignore rest))
  (format s "~a=~a" (car param) (cdr param)))

(defun create-signature (method url parameters)
  "Create an OAuth signature for a request.
This requires at least the *oauth-api-secret* to be bound properly, and
usually the *oauth-access-secret* as well.

According to spec https://dev.twitter.com/docs/auth/creating-signature"
  (assert (not (null *oauth-api-secret*)) (*oauth-api-secret*)
          'oauth-parameter-missing :parameter '*oauth-api-secret*)
  (let ((prepared-parameters (sort (loop for (key . val) in parameters
                                         collect (cons (url-encode key)
                                                       (url-encode val)))
                                   #'string< :key #'car)))
    (let ((parameter-string (format NIL "~{~/south::signature-format-parameter/~^&~}" prepared-parameters)))
      (let ((base (format NIL "~a&~a&~a" (string-upcase method) (url-encode url) (url-encode parameter-string)))
            (signing-key (format NIL "~a&~a" *oauth-api-secret* (or *oauth-access-secret* ""))))
        (hmac base signing-key)))))

(defun make-signed (method url oauth-parameters &optional other-parameters)
  "Returns the signed version of the oauth-parameters.
Simply generates a signature and appends the proper parameter."
  (cons (cons "oauth_signature" (create-signature method url (append oauth-parameters other-parameters)))
        oauth-parameters))

(defun authorization-format-parameter (s param &rest rest)
  (declare (ignore rest))
  (format s "~a=~s" (url-encode (car param)) (url-encode (cdr param))))

(defun create-authorization-header-value (parameters)
  "Turns the OAuth parameters into the correct header value."
  (format NIL "OAuth ~{~/south::authorization-format-parameter/~^, ~}"
          (sort parameters #'string< :key #'car)))

(defun create-authorization-header (method request-url oauth-parameters parameters)
  (assert (not (null *oauth-api-key*)) (*oauth-api-key*)
          'oauth-parameter-missing :parameter '*oauth-api-key*)
  (assert (not (null *oauth-signature-method*)) (*oauth-signature-method*)
          'oauth-parameter-missing :parameter '*oauth-signature-method*)
  (assert (not (null *oauth-version*)) (*oauth-version*)
          'oauth-parameter-missing :parameter '*oauth-version*)
  (let* ((oauth-parameters (append
                            oauth-parameters
                            `(("oauth_consumer_key" . ,*oauth-api-key*)
                              ("oauth_nonce" . ,(generate-nonce))
                              ("oauth_signature_method" . ,*oauth-signature-method*)
                              ("oauth_timestamp" . ,(write-to-string (get-unix-time)))
                              ("oauth_version" . ,*oauth-version*))
                            (when *oauth-access-token* `(("oauth_token" . ,*oauth-access-token*)))))
         (oauth-parameters (make-signed method request-url oauth-parameters parameters)))
    `(("Authorization" . ,(create-authorization-header-value oauth-parameters)))))

(defun request-wrapper (uri &rest drakma-params)
  (let* ((drakma:*text-content-types* (cons '("application" . "x-www-form-urlencoded")
                                            drakma:*text-content-types*))
         (vals (multiple-value-list (apply #'drakma:http-request uri
                                           :external-format-in *external-format*
                                           :external-format-out *external-format*
                                           :url-encoder #'url-encode
                                           drakma-params))))
    (if (< (nth 1 vals) 400)
        (values-list vals)
        (error 'oauth-request-error
               :body (nth 0 vals) :status (second vals) :headers (third vals)
               :url uri
               :method (getf drakma-params :method)
               :parameters (getf drakma-params :parameters)
               :sent-headers (getf drakma-params :additional-headers)))))

(defun signed-request (request-url &key parameters oauth-parameters additional-headers (method :POST) drakma-params)
  "Issue a signed request against the API.
This requires the *oauth-api-key*, *oauth-signature-method*,
*oauth-version* and at least *oauth-api-secret* to be set.
See CREATE-SIGNATURE.
For return values see DRAKMA:HTTP-REQUEST

According to spec https://dev.twitter.com/docs/auth/authorizing-request"
  (apply #'request-wrapper request-url
         :method method :parameters parameters
         :additional-headers (append additional-headers
                                     (create-authorization-header method request-url oauth-parameters parameters))
         drakma-params))

(defun prepare-data-parameters (parameters)
  (mapc #'(lambda (param)
            (unless (listp (cdr param))
              (setf (cdr param)
                    (list (etypecase (cdr param)
                            (pathname (cdr param))
                            ((array (unsigned-byte 8) (*)) (cdr param)))
                          :content-type "application/octet-stream"))))
        parameters))

(defun signed-data-request (request-url &key data-parameters parameters oauth-parameters additional-headers (method :POST) drakma-params)
  "Issue a signed data request against the API.
See SIGNED-REQUEST.

According to spec https://dev.twitter.com/docs/uploading-media"  
  (let ((parameters (append parameters (prepare-data-parameters data-parameters))))
    (apply #'request-wrapper request-url
           :method method :parameters parameters :form-data T
           :additional-headers (append additional-headers
                                       (create-authorization-header method request-url oauth-parameters NIL))
           drakma-params)))

(defun signed-data-parameters-request (request-url &key data-parameters parameters oauth-parameters additional-headers (method :POST) drakma-params)
  "Issue a signed data request against the API.
See SIGNED-REQUEST. The difference to SIGNED-DATA-REQUEST is that 
the default PARAMETERS are used to calculate the authorization header,
whereas the DATA-PARAMETERS are not. In SINGED-DATA-REQUEST, neither
of the parameters are used for the header."  
  (apply #'request-wrapper request-url
         :method method :parameters (append parameters (prepare-data-parameters data-parameters)) :form-data T
         :additional-headers (append additional-headers
                                     (create-authorization-header method request-url oauth-parameters parameters))
         drakma-params))

(defun signed-stream-request (request-url &key parameters oauth-parameters additional-headers (method :POST) drakma-params)
  "Issue a signed data request against the API.
See SIGNED-REQUEST. Returns values according to DRAKMA:HTTP-REQUEST with :WANT-STREAM T"
  (apply #'drakma:http-request request-url
         :method method :parameters parameters :want-stream T
         :additional-headers (append additional-headers
                                     (create-authorization-header method request-url oauth-parameters parameters))
         drakma-params))

(defun oauth/request-token (callback)
  "Query for a request token using the specified callback.
Returns an ALIST containing :OAUTH-TOKEN, :OAUTH-TOKEN-SECRET and 
:OAUTH-CALLBACK-CONFIRMED, the first two being strings and the last a boolean.

According to spec https://dev.twitter.com/docs/auth/implementing-sign-twitter"
  (oauth-response->alist
   (signed-request *oauth/request-token* :oauth-parameters `(("oauth_callback" . ,callback)))
   `((:OAUTH-CALLBACK-CONFIRMED . ,#'parse-boolean))))

(defun pin-request-token ()
  "Query for a PIN based request token.
Seee CALLBACK-REQUEST-TOKEN.

According to spec https://dev.twitter.com/docs/auth/pin-based-authorization"
  (oauth/request-token "oob"))

(defun oauth/authenticate (callback-url)
  "Initiate the authentication through the redirect mechanism.
Returns an URL that the user has to open in the browser.
Upon successful authentication, the page should redirect back
to the specified callback url. This callback endpoint should then
pass the proper parameters to COMPLETE-AUTHENTICATION.

According to spec https://dev.twitter.com/docs/auth/implementing-sign-twitter"
  (let ((data (oauth/request-token callback-url)))
    (setf *oauth-access-token* (cdr (assoc :oauth-token data))
          *oauth-access-secret* (cdr (assoc :oauth-token-secret data)))
    (format NIL "~a?oauth_token=~a" *oauth/authenticate* *oauth-access-token*)))

(defun oauth/authorize ()
  "Initiate the authentication through the PIN mechanism.
Returns an URL that the user has to open in the browser.
This page should, upon successful authentication, return a PIN
that has to be initialized by passing it to COMPLETE-AUTHENTICATION.

According to spec https://dev.twitter.com/docs/auth/pin-based-authorization"
  (let ((data (pin-request-token)))
    (setf *oauth-access-token* (cdr (assoc :oauth-token data))
          *oauth-access-secret* (cdr (assoc :oauth-token-secret data)))
    (format NIL "~a?oauth_token=~a" *oauth/authorize* *oauth-access-token*)))

(defun initiate-server-authentication ()
  "Initiate the authentication through the server mechanism.
This is the same as the redirect mechanism, except it starts up
a Hunchentoot server automatically and handles the callback for
you. The server will be started on *SERVER-PORT* and will be shut
down automatically after a single request."
  (assert (asdf:find-system "hunchentoot" NIL) () "Please install hunchentoot before using this authentication method.")
  (asdf:load-system "hunchentoot")
  (labels ((ht-symb (name)
             (find-symbol name "HUNCHENTOOT"))
           (ht-func (name &rest params)
             (apply (symbol-function (ht-symb name)) params)))
    (let ((server (ht-func "START" (make-instance (ht-symb "EASY-ACCEPTOR") :port *server-port*)))
          (standard-output *standard-output*))
      (handler-bind
          ((error #'(lambda (err) (declare (ignore err)) (ht-func "STOP" server))))
        (flet ((dispatcher (request)
                 (unless (cl-ppcre:scan  "favicon\.ico$" (ht-func "SCRIPT-NAME" request))
                   (lambda (&rest args)
                     (declare (ignore args))
                     (unwind-protect
                          (progn
                            (complete-authentication
                             (ht-func "GET-PARAMETER" "oauth_verifier")
                             (ht-func "GET-PARAMETER" "oauth_token"))
                            (format standard-output "~&SOUTH: Authentication completed.~%")
                            (let ((stream (ht-func "SEND-HEADERS")))
                              (write-sequence (flexi-streams:string-to-octets "SOUTH: Authentication completed.") stream)
                              (finish-output stream)))
                       (format standard-output "~&SOUTH: Shutting down server.~%")
                       (ht-func "STOP" server))))))
          (push #'dispatcher (symbol-value (ht-symb "*DISPATCH-TABLE*")))
          (oauth/authenticate (format NIL "http://localhost:~d/callback" *server-port*)))))))

(defun initiate-authentication (&key (method :SERVER) (api-key *oauth-api-key*) (api-secret *oauth-api-secret*))
  "Starts the authentication process and returns an URL that the user has to visit.
METHOD can be one of :PIN :SERVER or a string designating a callback URL.
Note that the :PIN method is NOT STANDARD and thus might not be available for your service.
See OAUTH/AUTHORIZE, INITIATE-SERVER-AUTHENTICATION and OAUTH/AUTHENTICATE respectively."
  (setf *oauth-api-key* api-key
        *oauth-api-secret* api-secret
        *oauth-access-token* NIL
        *oauth-access-secret* NIL)
  (case method
    (:PIN (oauth/authorize))
    (:SERVER (initiate-server-authentication))
    (T (oauth/authenticate method))))

(defun oauth/access-token (verifier)
  "Turn the tokens received through the authentication into an access token.

According to spec https://dev.twitter.com/docs/auth/implementing-sign-twitter"
  (oauth-response->alist
   (signed-request *oauth/access-token* :parameters `(("oauth_verifier" . ,verifier)))))

(defun complete-authentication (verifier &optional (token *oauth-access-token*))
  "Finishes the authentication procedure by retrieving the access token.
Sets the *OAUTH-TOKEN* and *OAUTH-TOKEN-SECRET* to their respective values."
  (setf *oauth-access-token* token)
  (let ((data (oauth/access-token verifier)))
    (setf *oauth-access-token* (cdr (assoc :oauth-token data))
          *oauth-access-secret* (cdr (assoc :oauth-token-secret data)))
    (funcall *authentication-callback* *oauth-access-token* *oauth-access-secret*)
    (values *oauth-access-token* *oauth-access-secret*)))
