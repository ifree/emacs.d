;;; init-fonts.el --- Font!

;;; Commentary:
;; 

;;; Code:

;;; Changing font sizes

(require-package 'default-text-scale)
(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)

(when (member "Inconsolata" (font-family-list nil))
    (set-face-attribute 'default nil :family "Inconsolata" :height 140))

(provide 'init-fonts)

;;; init-fonts.el ends here
