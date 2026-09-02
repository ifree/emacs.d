;;; window-config.el --- Window management  -*- lexical-binding: t; -*-

;;; Commentary:
;; Window history, selection, and predictable split behavior.

;;; Code:
(winner-mode 1)
(windmove-default-keybindings)

(defun frei/make-split-command (split-function)
  "Return a command that splits with SPLIT-FUNCTION and selects the new window."
  (lambda (&optional stay)
    (interactive "P")
    (let ((new-window (funcall split-function)))
      (set-window-buffer new-window (other-buffer))
      (unless stay
        (select-window new-window)))))

(defun frei/split-right-instead ()
  "Replace the current layout with a left-right split."
  (interactive)
  (delete-other-windows)
  (funcall (frei/make-split-command #'split-window-right)))

(defun frei/split-below-instead ()
  "Replace the current layout with a top-bottom split."
  (interactive)
  (delete-other-windows)
  (funcall (frei/make-split-command #'split-window-below)))

(global-set-key
 (kbd "C-x 2")
 (frei/make-split-command #'split-window-below))

(global-set-key
 (kbd "C-x 3")
 (frei/make-split-command #'split-window-right))

(use-package switch-window
  :ensure t
  :bind
  (("C-x o" . switch-window)
   ("C-x |" . frei/split-right-instead)
   ("C-x _" . frei/split-below-instead))
  :custom
  (switch-window-shortcut-style 'alphabet)
  (switch-window-timeout nil))

(provide 'window-config)

;;; window-config.el ends here
