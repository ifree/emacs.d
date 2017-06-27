;;; cpp-config.el --- cpp config

;;; Commentary:
;; 


;;; Code:
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(use-package eshell
  :init
  (add-hook 'eshell-mode-hook 'compilation-shell-minor-mode)
;  (add-hook 'eshell-after-prompt-hook 'eshell-parse-compilation)
  (defun eshell-adjust (&optional arg)
    (interactive)
    (let* ((buf-name (cond ((numberp arg)
			    (format "%s<%d>"
				    eshell-buffer-name
				    arg))
			   (t
			    eshell-buffer-name)
			   ))
	   (buf (get-buffer buf-name))
	   (eshell-win (get-buffer-window buf)))
      ;; if buffer is nil or in background, split window vertically
      (if (not (and eshell-win buf))
	  (let ((win (split-window-below -8))
		;; eshell default dir
		(default-directory (if (projectile-project-p) (projectile-project-root) default-directory)))
	    (select-window win)
	    (eshell arg))
	(select-window eshell-win)))))

(use-package cmake-mode
  :ensure t)

(use-package rtags
  :ensure t
  :init
  (setq rtags-autostart-diagnostics t)
  (setq rtags-completions-enabled t)
  :config
  (rtags-enable-standard-keybindings))

(use-package company-rtags
  :ensure t)

(use-package irony-eldoc
  :ensure t)

(use-package irony
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook #'irony-eldoc)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  ;; Windows performance tweaks
  ;;
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  ;; Set the buffer size to 64K on Windows (from the original 4K)
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024))))

(use-package company-irony
  :ensure t
  :init
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

(use-package company-irony-c-headers
  :ensure t)

(use-package company
  :ensure t
  :config
  ;; delete semantic backend
  (setq company-backends (delete 'company-semantic company-backends))
  
  (add-to-list
   'company-backends 'company-rtags)
  (add-to-list
   'company-backends 'company-irony)
  (add-to-list
   'company-backends 'company-irony-c-headers))

(use-package flycheck-irony
  :ensure t)

(use-package flycheck
  :init
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  :config
;  (flycheck-select-checker 'rtags)
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package cmake-ide
  :ensure t
  :config
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
    (cmake-ide--mode-hook))

  (add-hook 'c-mode-hook #'my-cmake-ide-setup)
  (add-hook 'c++-mode-hook #'my-cmake-ide-setup))


;; TODO: clang format
;; TODO: projectile-find-file-at-point

(provide 'cpp-config)

;;; cpp-config.el ends here
