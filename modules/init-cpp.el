;;; init-cpp.el --- c++ configuration

;;; Commentary:
;; 


;;; Code:
(require-package 'irony)
(require-package 'irony-eldoc)
(require-package 'company-irony)
(require-package 'company-c-headers)
(require-package 'flycheck-irony)
(require-package 'cmake-mode)
(require-package 'rtags)
(require-package 'sr-speedbar)
(require-package 'function-args)

(require 'compile)
(require 'sr-speedbar)
(require 'eshell)

(unless (boundp 'byte-compile-not-obsolete-vars)
    (defvar byte-compile-not-obsolete-vars nil))
(require 'function-args)
(require 'cmake-tool)

;;; cmake tools
(add-hook 'before-save-hook 'cmake-tool-before-file-saved)
(add-hook 'kill-buffer-hook 'cmake-tool-before-file-deleted)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; fa
(fa-config-default)

;;; company-rtags
;; (add-to-list 'company-backends 'company-rtags)
;; (setq company-rtags-begin-after-member-access t)
;; (setq rtags-completions-enabled t)
;; (rtags-diagnostics)

;;; speedbar
(setq speedbar-show-unknown-files t)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-width 30)

(defun sr-speedbar-skip-eshell ()
  "Don't open file in eshell"
  (let ((wins (window-list)))
    (car (remove-if (lambda (itm)
                      (or
                       (string=
                        eshell-buffer-name (buffer-name (window-buffer itm)))
                       (eq sr-speedbar-window itm)))
                    wins))))

(defun sr-speedbar-before-visiting-file-hook ()
  "Function that hooks `speedbar-before-visiting-file-hook'."
  (select-window (sr-speedbar-skip-eshell)))

(defun sr-speedbar-before-visiting-tag-hook ()
  "Function that hooks `speedbar-before-visiting-tag-hook'."
  (select-window (sr-speedbar-skip-eshell)))

(defun sr-speedbar-visiting-file-hook ()
  "Function that hooks `speedbar-visiting-file-hook'."
  (select-window (sr-speedbar-skip-eshell)))

(defun sr-speedbar-visiting-tag-hook ()
  "Function that hooks `speedbar-visiting-tag-hook'."
  (select-window (sr-speedbar-skip-eshell)))



;(setq sr-speedbar-auto-refresh t)
;(setq speedbar-use-images nil)

;;; irony
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
; irony company
(eval-after-load 'company
  '(progn
    (add-to-list 'company-backends 'company-irony)
    (add-to-list 'company-backends 'company-c-headers)))
; flycheck
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
; hook
(defun irony-mode-setup-hook ()
  (add-to-list 'irony-supported-major-modes 'pg-mode)
  
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async)
  ;;add build to search pth
  (when (functionp 'projectile-project-root)
    (let* ((proj-root (projectile-project-root))
          (comp-file (concat (file-name-as-directory "build")
                             "compile_commands.json")))
      (when (file-exists-p (concat proj-root comp-file))
        (irony-cdb-json-add-compile-commands-path proj-root
                                                  comp-file)

        ;; rtags stuff
        (shell-command (concat "rc -J " proj-root "build"))
        (message (concat "rc -J " proj-root "build")))))

  (irony-cdb-autosetup-compile-options)
  (eldoc-mode)
  (irony-eldoc)
  (company-irony-setup-begin-commands))

(add-hook 'irony-mode-hook 'irony-mode-setup-hook)

;;; rtags
(rtags-enable-standard-keybindings)


(defun smart-compile()
  (interactive)
  ;; 查找 Makefile
  (let ((candidate-make-file-name '("makefile" "Makefile" "GNUmakefile" "SConstruct"))
        command)
    (if (not (null
              (find t candidate-make-file-name :key
                    '(lambda (f) (file-readable-p f)))))
	(if (file-exists-p "SConstruct")
	    (setq command "scons")
	  (setq command "make -k "))
      ;; 没有找到 Makefile ，查看当前 mode 是否是已知的可编译的模式
      (if (null (buffer-file-name (current-buffer)))
	  (message "Buffer not attached to a file, won't compile!")
	(if (eq major-mode 'c-mode)
	    (setq command
		  (concat "gcc -Wall -o "
			  (file-name-sans-extension
			   (file-name-nondirectory buffer-file-name))
			  " "
			  (file-name-nondirectory buffer-file-name)
			  " -g -lm "))
	  (if (eq major-mode 'c++-mode)
	      (setq command
		    (concat "g++ -Wall -o "
			    (file-name-sans-extension
			     (file-name-nondirectory buffer-file-name))
			    " "
			    (file-name-nondirectory buffer-file-name)
			    " -g -lm "))
	    (message "Unknow mode, won't compile!")))))
    (if (not (null command))
        (let ((command (read-from-minibuffer "Compile command: " command)))
          (compile command)))))


;;; keybindings
(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-map (kbd "<f5>") 'smart-compile)
     (define-key c++-mode-map (kbd "<f5>") 'smart-compile)
     (define-key c-mode-map (kbd "<f6>") 'recompile)
     (define-key c++-mode-map (kbd "<f6>") 'recompile)
     (define-key c-mode-map   (kbd "M-<up>") 'ff-find-other-file)
     (define-key c++-mode-map (kbd "M-<up>") 'ff-find-other-file)))

;; clang format
(defun reformat-region (&optional b e)
  (interactive "r")
  (when (not (buffer-file-name))
    (error "A buffer must be associated with a file in order to use REFORMAT-REGION."))
  (when (not (executable-find "clang-format"))
    (error "clang-format not found."))
  (shell-command-on-region b e
			   "clang-format -style=LLVM"
			   (current-buffer) t)
  (indent-region b e))

;;; gdb
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t)

;;; eshell
;;eshell-load-hook
;;; todo
(add-hook 'eshell-mode-hook 'compilation-shell-minor-mode)
(add-hook 'eshell-after-prompt-hook 'eshell-parse-compilation)

(defun eshell-parse-compilation ()
  (save-excursion
    (backward-char 5)
    (let ((pt (search-backward-regexp eshell-prompt-regexp (point-min) t)))
      (when pt
        (compilation--flush-parse pt (point))))))


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
        (let ((win (split-window-below -8)))
          (select-window win)
          (eshell arg)
          )
      (select-window eshell-win))))

(provide 'init-cpp)

;;; init-cpp.el ends here
