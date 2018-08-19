;;; cpp-config.el --- cpp config

;;; Commentary:
;; 


;;; Code:
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(use-package eshell
  :bind
  (:map global-map
        ([f9]        . eshell-adjust))
  :init
  (add-hook 'eshell-mode-hook 'compilation-shell-minor-mode)
;  (add-hook 'eshell-after-prompt-hook 'eshell-parse-compilation)
  (defun eshell-adjust (&optional arg)
    (interactive)
    (let* ((buf-name (if (boundp 'eshell-buffer-name)
			 (cond ((numberp arg)
				   (format "%s<%d>"
					   eshell-buffer-name
					   arg))
				  (t
				   eshell-buffer-name))
		       nil))
	   (buf (if buf-name (get-buffer buf-name)))
	   (eshell-win (if buf (get-buffer-window buf))))
      ;; if buffer is nil or in background, split window vertically
      (if (not (and eshell-win buf))
	  (let ((win (split-window-below -8))
		;; eshell default dir
		(default-directory (if (projectile-project-p) (projectile-project-root) default-directory)))
	    (select-window win)
	    (eshell arg))
	(select-window eshell-win)))))

(use-package google-c-style
  :ensure t
  :init
  (add-hook 'c-mode-common-hook 'google-set-c-style))

(use-package cmake-mode
  :ensure t)

(use-package cmake-ide
  :ensure t
  :init
  (use-package projectile :ensure t)
;  (cmake-ide-setup)
  (defun my-cmake-ide-setup ()
    ;; if current file is inside project, set correct path
    (setq flycheck-gcc-language-standard "c++11")
    (when (projectile-project-p)
	(put 'cmake-ide-project-dir 'safe-local-variable 'stringp)
	(put 'cmake-ide-build-dir 'safe-local-variable 'stringp)
	(setq cmake-ide-project-dir (projectile-project-root))
	(setq cmake-ide-build-dir (concat cmake-ide-project-dir "build")))
    (cmake-ide-setup))

  (add-hook 'c-mode-hook #'my-cmake-ide-setup)
  (add-hook 'c++-mode-hook #'my-cmake-ide-setup)
  :bind
  ([f7] . cmake-ide-compile))

;;; lsp
;;; or clangd later
(defun cquery//enable ()  
  (condition-case nil
      (lsp-cquery-enable)
    (user-error nil)))

;;; lsp-ui-flycheck

(use-package cquery
  :ensure t
  :after lsp-mode
  :commands lsp-cquery-enable
  :init
  (add-hook 'c-mode-hook #'cquery//enable)
  (add-hook 'c++-mode-hook #'cquery//enable))

(use-package company-lsp
  :ensure t
  :after lsp-mode
  :config
  (progn
    (push 'company-lsp company-backends)
    (setq company-transformers nil
	  company-lsp-async t
	  company-lsp-cache-candidates nil)))


;;; gdb
(use-package gdb-mi
  :init
  (setq
   ;; use gdb-many-windows by default
   gdb-many-windows t
   ;; Non-nil means display source file containing the main routine at startup
   gdb-show-main t))


;; TODO: clang format
;; TODO: projectile-find-file-at-point(maybe `rtags-find-file` is enough)

(provide 'cpp-config)

;;; cpp-config.el ends here
