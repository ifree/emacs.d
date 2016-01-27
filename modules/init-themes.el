;;; init-themes.el --- Custom themes

;;; Commentary:
;; 


;;; Code:
(require-package 'zenburn-theme)
(require-package 'solarized-theme)
(require-package 'monokai-theme)

(setq-default custom-enabled-themes '(monokai))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

(provide 'init-themes)

;;; init-themes.el ends here
