;;; init-clojure.el --- clojure dev configs

;;; Commentary:
;; 


;;; Code:

(when (maybe-require-package 'clojure-mode)
  (require-package 'cljsbuild-mode)
  (require-package 'elein)

  (after-load 'clojure-mode
    (add-hook 'clojure-mode-hook 'frei/lisp-setup)
    (add-hook 'clojure-mode-hook 'subword-mode)))

(when (maybe-require-package 'cider)
  (require-package 'ac-cider)

  (setq nrepl-popup-stacktraces nil)

  (after-load 'cider
    (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
    (add-hook 'cider-mode-hook 'ac-cider-setup)
    (after-load 'auto-complete
      (add-to-list 'ac-modes 'cider-repl-mode))

    (add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
    (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'subword-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode)
    (define-key cider-mode-map (kbd "C-c C-d") 'ac-cider-popup-doc)

    ;; nrepl isn't based on comint
    (add-hook 'cider-repl-mode-hook
              (lambda () (setq show-trailing-whitespace nil))))

  (require-package 'flycheck-clojure)
  (after-load 'clojure-mode
    (after-load 'flycheck
      (flycheck-clojure-setup))))

(provide 'init-clojure)

;;; init-clojure.el ends here
