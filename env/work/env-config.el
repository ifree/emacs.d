;;; env-config.el --- home config

;;; Commentary:
;; 


;;; Code:
(load (expand-file-name "env/default/env-config.el" user-emacs-directory) nil t)

(use-package coding-config :demand)
(use-package cpp-config :demand)
(use-package rust-config :demand)

;;; cargo
(when (memq window-system '(w32))
  (setq cargo-process--custom-path-to-bin (format "%s/mingw64/bin" (getenv "msys_home"))))

(setq racer-rust-src-path (expand-file-name "~/Source/rust/src")) ;; Rust source code PATH


(provide 'env-config)

;;; env-config.el ends here
