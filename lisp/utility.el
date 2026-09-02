;;; utility.el --- Small editor utilities  -*- lexical-binding: t; -*-

;;; Commentary:
;; File-oriented commands kept for compatibility with the old configuration.

;;; Code:
(require 'subr-x)
(require 'browse-url)

(defun frei/delete-current-file ()
  "Move the current buffer file to the trash and kill its buffer."
  (interactive)
  (let ((file buffer-file-name))
    (unless file
      (user-error "The current buffer is not visiting a file"))
    (when (yes-or-no-p
           (format "Move %s to the trash? "
                   (file-name-nondirectory file)))
      (delete-file file t)
      (kill-buffer (current-buffer)))))

(defun frei/rename-current-file (new-name)
  "Rename the current buffer file to NEW-NAME."
  (interactive
   (list
    (read-file-name
     "Rename file to: "
     (and buffer-file-name
          (file-name-directory buffer-file-name))
     buffer-file-name)))
  (if (fboundp 'rename-visited-file)
      (rename-visited-file new-name)
    (let ((old-name buffer-file-name))
      (unless old-name
        (user-error "The current buffer is not visiting a file"))
      (rename-file old-name new-name)
      (set-visited-file-name new-name t t))))

(defun frei/browse-current-file ()
  "Open the current buffer file with the system browser."
  (interactive)
  (unless buffer-file-name
    (user-error "The current buffer is not visiting a file"))
  (browse-url-of-file buffer-file-name))

(defun frei/browse-current-folder ()
  "Open the current buffer directory with the system browser."
  (interactive)
  (let ((directory
         (if buffer-file-name
             (file-name-directory buffer-file-name)
           default-directory)))
    (browse-url-of-file directory)))

(defun frei/create-parent-directory ()
  "Offer to create the parent directory of a newly visited file."
  (when-let* ((file buffer-file-name)
              (directory (file-name-directory file)))
    (when (and (not (file-directory-p directory))
               (y-or-n-p
                (format "Create missing directory %s? " directory)))
      (make-directory directory t)))
  nil)

(add-hook 'find-file-not-found-functions
          #'frei/create-parent-directory)

(defalias 'delete-this-file #'frei/delete-current-file)
(defalias 'rename-this-file-and-buffer #'frei/rename-current-file)
(defalias 'browse-current-file #'frei/browse-current-file)
(defalias 'browse-current-folder #'frei/browse-current-folder)

(provide 'utility)

;;; utility.el ends here
