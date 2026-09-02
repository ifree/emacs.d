;;; utility.el --- Small editor utilities  -*- lexical-binding: t; -*-

;;; Commentary:
;; File-oriented commands without a built-in counterpart.  Renaming is
;; covered by `rename-visited-file' and opening the current file in a
;; browser by `browse-url-of-buffer'.

;;; Code:
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

(defun frei/browse-current-folder ()
  "Open the current buffer directory with the system browser."
  (interactive)
  (let ((directory
         (if buffer-file-name
             (file-name-directory buffer-file-name)
           default-directory)))
    (browse-url-of-file directory)))

(provide 'utility)

;;; utility.el ends here
