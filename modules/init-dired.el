;;; init-dired.el --- dired enhancement

;;; Commentary:
;; 


;;; Code:
(require-package 'dired+)
(require-package 'dired-sort)

(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(after-load 'dired
  (require 'dired+)
  (require 'dired-sort)
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (add-hook 'dired-mode-hook
            (lambda () (guide-key/add-local-guide-key-sequence "%"))))


(defun dired-open-in-external-app ()
  "Open the current file or dired marked files in external app."
  (interactive)
  (let ( confirmed
         (file-list
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           (t (list (buffer-file-name))))))

    (setq confirmed (if (<= (length file-list) 5)
			t
		      (y-or-n-p "Open more than 5 files ? ")))
    (when confirmed
      (cond
       (*is-win*
        (mapc (lambda (path) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" path t t)) ) file-list))
       (*is-mac*
        (mapc (lambda (path) (shell-command (format "open \"%s\"" path)) )  file-list))
       (*is-linux*
        (mapc (lambda (path) (shell-command (format "xdg-open \"%s\"" path))) file-list))))))

(when (maybe-require-package 'diff-hl)
  (after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(provide 'init-dired)

;;; init-dired.el ends here
