;;; init-blogging.el --- blogging stuff

;;; Commentary:
;; write blog using org-mode
;; tumblr api
;; https://www.tumblr.com/docs/en/api/v2

;;; Code:

;;; tumblr
(require-package 'tumblesocks)
(require 'tumblesocks)
(require 'flickr)
(setq tumblesocks-blog "programming4fun.tumblr.com")

(setq oauth-nonce-function 'oauth-internal-make-nonce)
;(setq oauth-use-curl t)
;(setq oauth-curl-insecure t)

(defun ifree-org-html-get-body (&optional buffer)
  "Export BUFFER to html body."
  (let ((html-content))
    (save-excursion
      (org-html-export-as-html nil nil t t)
      (with-current-buffer "*Org HTML Export*"
        (setq html-content
              (buffer-substring-no-properties (point-min) (point-max)))
        (kill-buffer-and-window)))
    html-content))

(defun ifree-org-html-parse-buffer (&optional buffer keywords)
  "Convert BUFFER content to html."
  (interactive)
  (let* ((props (org-element-parse-buffer 'greater-element t)))
    (org-element-map props 'keyword
      (lambda (item)
        (if (member (org-element-property :key item)
                    (or keywords '("TITLE" "KEYWORDS" "DATE" "STATE" "BLOG_ID")))
            
            `(,(org-element-property :key item)
              ,(org-element-property :value item))
          nil
          )))))

(defun ifree-org-set-option-value (option value &optional append)
  "Set OPTION value with VALUE, when there are no such OPTION, you can APPEND."
  (let ((re "#\\+\\([^\:]+\\)\\:\s*\\(.*\\)$")
        found)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (when (string= (match-string 1) option)
          (setq found t)
          (replace-match (concat "#+\\1: " value))
          (goto-char (match-end 2))
          ))
      (when (and (not found) append)
          (insert (format "\n#+%s: %s" option value))))))

(defun ifree-update-article-time (&optional time)
  "Update article's last edition TIME."
  (interactive)
  (ifree-org-set-option-value "DATE"
                              (format-time-string "%Y-%m-%d %T GMT" time t)))


(defun ifree-format-article (begin end)
  "Add space between Chinese and English letters between BEGIN and END."
  (interactive "r")
  (let (region-begin
        region-end
        (matcher-begin "\\(?1:[\u4e00-\u9fa5]+\\)\\(?2:[a-zA-Z]+\\)")  ;cjk,
                                                                       ;symbols?
        (matcher-end "\\(?1:[a-zA-Z]+\\)\\(?2:[\u4e00-\u9fa5]+\\)"))
    
    (if (region-active-p)
        (setq region-begin begin region-end end)
      (setq region-begin (point-min) region-end (point-max)))
    
    (save-excursion
      (goto-char region-begin)
      (while (re-search-forward matcher-begin region-end t)
        (replace-match "\\1 \\2")
        (setq region-end (+ 1 region-end)))
      (goto-char region-end)
      (while (re-search-backward matcher-end region-begin t)
        (replace-match "\\1 \\2")))))


(defun ifree-org-replace-image-with-flickr ()
  "Upload all file links to flickr then replace file link withimage url.
refer `org-display-inline-image`."
  (interactive)
  (let ((re (concat "\\[\\[\\(\\(file:\\)\\|\\([./~]\\)\\)\\([^]\n]+?"
			  (substring (org-image-file-name-regexp) 0 -2)
			  "\\)\\]\\]"))
        file
        photo-id photo-url photo-page
        (case-fold-search t)
        )
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (setq file (expand-file-name
                    (concat (or (match-string 3) "") (match-string 4))))

        (when (file-exists-p file)
          (save-match-data
            (setq photo-id (flickr-upload-image file)
                  photo-url (flickr-get-photo-url photo-id)
                  photo-page (flickr-get-photo-page photo-id)
                  ))
          
          (replace-match (format
                          (concat
                           "@@html:"
                           "<a href=\"%s\"> <img src=\"%s\" width=\"640\" height=\"360\" alt=\"ifree's flickr image\"></a>"
                           "@@")
                          photo-page
                          photo-url
                          ))
          (goto-char (match-beginning 1)))))))


(defun ifree-org-blog-readon()
  (interactive)
  (insert "@@html:<!-- break -->@@"))

(defun ifree-org-blog-gist()
  (interactive)
  (let
      ((id
        (if mark-active
            (buffer-substring (region-beginning) (region-end))
          (read-string "input gist id: "))))
    (when mark-active
        (delete-region  (region-beginning) (region-end)))
    (insert 
     (format 
      "@@html:<script src=\"https://gist.github.com/%s.js\"></script>@@"
      id
      ))))

(defun tumblesocks-text-post-from-org (&optional edit)
  "Create or edit a post with TITLE from `org-mode` buffer. Note
when edit a post you can change it state from draft to publish."
  (interactive "P")
  (let* ((props (ifree-org-html-parse-buffer))
         (args `(:type "text"
                       :format "html"
                       :state ,(or (car (assoc-default "STATE" props)) (tumblesocks-get-post-state))
                       :body ,(ifree-org-html-get-body)
                       :title ,(car (assoc-default "TITLE" props))
                       :tags ,(car (assoc-default "KEYWORDS" props))
                       :date ,(car (assoc-default "DATE" props))
                       ))
         (post-func 'tumblesocks-api-new-post)
         )

    (if (and (string= (plist-get args :state) "schedule") (not edit))
        (progn
          (plist-put args :state "queue")
          (plist-put args :publish_on (read-string "Publish On: "))))

    (print props)
    (when edit
      (ifree-update-article-time)
      (setq post-func (lambda (args)
                        (tumblesocks-api-edit-post
                         (car (assoc-default "BLOG_ID" props)) args))))

    (let* ((blog-url
            (plist-get (plist-get (tumblesocks-api-blog-info) :blog) :url))
           (new-post-id (format "%d" (plist-get (funcall post-func args)
                                                :id)))
           (new-post-url
            (let* ((last-char (substring blog-url -1)))
              (cond ((string= last-char "/")
                     (concat blog-url new-post-id)) ; url has a trailing slash
                    (t (concat blog-url (concat "/" new-post-id)))))))
      ;; So we need to both return this ID
      ;; and copy the URL to the clipboard (and message it too.)
      ;; Thanks to tumble.el for this:
      (kill-new new-post-url)
      (if edit          
          (message (concat "Post updated at " new-post-url))
        (message (concat "New post created at " new-post-url)))
      (ifree-org-set-option-value "BLOG_ID" new-post-id t)
      new-post-id)))





(provide 'init-blogging)

;;; init-blogging.el ends here
