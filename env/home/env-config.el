;;; env-config.el --- home config

;;; Commentary:
;; 


;;; Code:
(load (expand-file-name "env/default/env-config.el" user-emacs-directory) nil t)

(use-package coding-config :demand)
(use-package cpp-config :demand)
(use-package org-config :demand)

(provide 'env-config)

;;; env-config.el ends here
