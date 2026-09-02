;;; theme-config.el --- Theme and frame appearance  -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep the existing Doom Molokai look with a lightweight built-in mode line.

;;; Code:
(require 'seq)

(use-package doom-themes
  :ensure t
  :demand t
  :config
  (load-theme 'doom-molokai t)
  (doom-themes-visual-bell-config))

(setq initial-scratch-message
      ";; Welcome home, good hunter. May Emacs help you!\n\n"
      use-file-dialog nil
      use-dialog-box nil
      indicate-empty-lines t
      frame-title-format
      '((:eval
         (if buffer-file-name
             (abbreviate-file-name buffer-file-name)
           "%b")))
      display-time-24hr-format t)

(add-to-list 'default-frame-alist '(width . 160))
(add-to-list 'default-frame-alist '(height . 60))

(defconst frei/font-candidates
  '("Inconsolata" "Cascadia Code" "Menlo" "DejaVu Sans Mono" "Consolas")
  "Monospace font families to try for the default face, in order.")

(defun frei/apply-default-font (&optional frame)
  "Use the first installed font of `frei/font-candidates' in graphical FRAME."
  (let ((target-frame (or frame (selected-frame))))
    (when (display-graphic-p target-frame)
      (with-selected-frame target-frame
        (let* ((installed (font-family-list target-frame))
               (font (seq-find (lambda (family) (member family installed))
                               frei/font-candidates)))
          (when font
            (set-face-attribute
             'default target-frame :family font :height 160)))))))

(frei/apply-default-font)
(add-hook 'after-make-frame-functions #'frei/apply-default-font)

(defun frei/adjust-opacity (increment)
  "Adjust the selected frame background opacity by INCREMENT."
  (interactive "nOpacity increment: ")
  (unless (display-graphic-p)
    (user-error "Opacity is only available in graphical frames"))
  (let* ((old-value
          (or (frame-parameter nil 'alpha-background) 100))
         (new-value
          (max frame-alpha-lower-limit
               (min 100 (+ old-value increment)))))
    (set-frame-parameter nil 'alpha-background new-value)))

(global-set-key
 (kbd "M-C-8")
 (lambda ()
   (interactive)
   (frei/adjust-opacity -2)))

(global-set-key
 (kbd "M-C-9")
 (lambda ()
   (interactive)
   (frei/adjust-opacity 2)))

(global-set-key
 (kbd "M-C-0")
 (lambda ()
   (interactive)
   (set-frame-parameter nil 'alpha-background 100)))

(display-time-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

(provide 'theme-config)

;;; theme-config.el ends here
