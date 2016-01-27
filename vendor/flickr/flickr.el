;;; flickr.el --- emacs flickr uploader

;;; Commentary:
;; Use new oauth authentication flow.
;;
;; ** Mainly copy from tumblesocks.

;;; Code:
(require 'oauth)
(require-package 'request)
(require 'request)

(defcustom flickr-consumer-key "dfe22453290e8de986b1d6f2d77f22e8"
  "Flickr OAuth consumer key."
  :type 'string
  :group 'flickr
  )

(defcustom flickr-consumer-secret "080e0987abd0c149"
  "Flickr OAuth consumer secret."
  :type 'string
  :group 'flickr
  )

(defcustom flickr-oauth-service-url "https://www.flickr.com/services/oauth/"
  "Flickr service url."
  :type 'string
  :group 'flickr
  )

(defcustom flickr-api-url-rest "https://api.flickr.com/services/rest/"
  "Flickr REST api url."
  :type 'string
  :group 'flickr
  )

(defcustom flickr-token-file (concat
                              (file-name-as-directory user-emacs-directory)
                              "flickr-oauth-token")
  "Where to store the token."
  :type 'file
  :group 'flickr
  )


(defvar flickr-token nil)

(defun flickr-forget-authencation ()
  "Forget your authencation info."
  (interactive)
  (setq flickr-token nil)
  (when (file-exists-p flickr-token-file)
    (delete-file flickr-token-file)
    )
  )

(defun flickr-authenticate ()
  "Sign oauth token."
  (interactive)
  (when (or (not flickr-consumer-key)
            (not flickr-consumer-secret))
    (error "You must set `flickr-consumer-key` and `flickr-consumer-secret`")
    )
  (when (file-exists-p flickr-token-file)
    (save-excursion
        (find-file flickr-token-file)
        (let ((str (buffer-substring (point-min) (point-max))))
          (if (string-match "\\([^:]*\\):\\(.*\\)"
                            (buffer-substring (point-min) (point-max)))
              (setq flickr-token
                    (make-oauth-access-token
                     :consumer-key flickr-consumer-key
                     :consumer-secret flickr-consumer-secret
                     :auth-t (make-oauth-t
                              :token (match-string 1 str)
                              :token-secret (match-string 2 str))))))
        (kill-this-buffer)))
  (unless flickr-token
    (setq flickr-token (oauth-authorize-app
                        flickr-consumer-key
                        flickr-consumer-secret
                        (concat flickr-oauth-service-url "request_token")
                        (concat flickr-oauth-service-url "access_token")
                        (concat flickr-oauth-service-url "authorize")
                        ))
    (save-excursion
      (find-file flickr-token-file)
      (erase-buffer)
      (let ((token (oauth-access-token-auth-t flickr-token)))
        (insert (format "%s:%s\n"
                        (oauth-t-token token)
                        (oauth-t-token-secret token))))
      (save-buffer)
      (kill-this-buffer))))


(defun base-58-encode (num)
  "Encode NUM to base58 encoded string."
  (let* ((alphabet "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ")
        (base-count (length alphabet))
         div
         mod
         result)
    
    (while (>= num base-count)
      (setq div (/ num base-count))
      (setq mod (- num (* base-count div)))
      (setq result (concat (char-to-string (aref alphabet mod)) result))
      (setq num div)
      )
    (when (> num 0)
        (setq result (concat (char-to-string (aref alphabet num)) result)))
    result))


(defun flickr-upload-image (file &optional title desc tags content_type)
  "Upload FILE to fllickr (other options are ignored..."
  (interactive (list (read-file-name "select image: " default-directory)))
  (unless file
    (error "You must select a file to upload!"))
  (let ((result
         (flickr-upload-image-internal "https://up.flickr.com/services/upload/"
                                       `(("photo" . ,file))
                                       )))
    result))


(defun flickr-make-request (url method)
  "Create oauth request object."
  (let ((req (oauth-make-request url
                                  (oauth-access-token-consumer-key flickr-token)
                                  (oauth-access-token-auth-t flickr-token))))

    (setf (oauth-request-http-method req) method)
    
    (when oauth-post-vars-alist
      (setf (oauth-request-params req)
            (append (oauth-request-params req) oauth-post-vars-alist)))    
    
    (oauth-sign-request-hmac-sha1
     req (oauth-access-token-consumer-secret flickr-token))
    
    req))


(defun flickr-get-photo-url (photo-id)
  "flickr.photos.getInfo"
  (let* ((oauth-request-params `(("method" .  "flickr.photos.getInfo")
                                 ("photo_id" . ,photo-id)
                                 ))
         (req (flickr-make-request flickr-api-url-rest "GET")))

    (let ((resp (request flickr-api-url-rest
                    :params oauth-request-params
                    :headers (oauth-request-to-header req)
                    :parser 'buffer-string
                    :sync t
                    ))
          ;https://farm4.staticflickr.com/3858/19446603522_000884b645_z.jpg
          (url "https://farm%s.staticflickr.com/%s/%s_%s_z.%s")
          (attr-re "=\"\\([^\"]*\\)\"")
          (get-attr (lambda (name)      ;orz
                      (save-match-data
                        (goto-char (point-min))
                        (if (re-search-forward (concat "\s" name attr-re) nil t)
                            (match-string 1)
                          nil))
                      ))
          )
      (when resp
        (with-temp-buffer
          (insert (request-response-data resp))
          (format url
                  (funcall get-attr "farm")
                  (funcall get-attr "server")
                  photo-id
                  (funcall get-attr "secret")                  
                  (funcall get-attr "originalformat")))))))

(defun flickr-get-photo-page (photo-id)
  "Get photo home page by PHOTO-ID."
  (concat "https://flic.kr/p/" (base-58-encode (string-to-number photo-id))))

;(setq request-log-level 'debug)

;(flickr-upload-image "/home/ifree/Media/Pictures/Wallpaper/goose.jpg")

(defun flickr-upload-image-internal (url files)
  "Upload FILES server."
  (let ((req (flickr-make-request url "POST"))
        headers
        response
        )
    
    (setq headers
          (oauth-request-to-header
           req))
    ;start upload
    (setq response
          (request url
                   :type "POST"
                   :data oauth-post-vars-alist
                   :files files
                   :headers headers
                   :parser 'buffer-string
                   :sync t
                   ))
    (when (eq (request-response-symbol-status response) 'success)
      (let ((data (request-response-data response)))
        (when data
          (let ((re "<photoid>\\([^<]+\\)")
                photo-id)
            (save-match-data
              (when (string-match "<photoid>\\([^<]+\\)" data)
                (setq photo-id
                      (match-string-no-properties 1 data))
                
                photo-id))))))))


(provide 'flickr)

;;; flickr.el ends here
