;;; init-projectile.el --- projectile config

;;; Commentary:
;; 


;;; Code:
(require-package 'projectile)

(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-remember-window-configs t)

(add-to-list 'projectile-project-root-files "project.properties")
(provide 'init-projectile)

;;; init-projectile.el ends here
