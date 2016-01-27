;;; init-javascript.el --- js

;;; Commentary:
;; 


;;; Code:
(require-package 'json)
(require-package 'js2-mode)
(require-package 'js2-refactor)
(require-package 'web-beautify)
(require-package 'js-comint)
(require-package 'rainbow-delimiters)
(require-package 'coffee-mode)

(defcustom preferred-javascript-mode
  'js2-mode
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(js2-mode js-mode))

(defconst preferred-javascript-indent-level 2)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl))
(setq auto-mode-alist (cons `("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))


;; js2-mode

;; Change some defaults: customize them to override
(setq-default js2-basic-offset 2
              js2-bounce-indent-p nil)
(after-load 'js2-mode
  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  ;; ... but enable it if flycheck can't handle javascript
  (autoload 'flycheck-get-checker-for-buffer "flycheck")
  (defun sanityinc/disable-js2-checks-if-flycheck-active ()
    (unless (flycheck-get-checker-for-buffer)
      (set (make-local-variable 'js2-mode-show-parse-errors) t)
      (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
  (add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)

  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))

  (after-load 'js2-mode
    (js2-imenu-extras-setup)))

;; js-mode
(setq-default js-indent-level preferred-javascript-indent-level)


(add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))


;; Javascript nests {} and () a lot, so I find this helpful

(require-package 'rainbow-delimiters)
(dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))



;;; Coffeescript

(after-load 'coffee-mode
  (setq coffee-js-mode preferred-javascript-mode
        coffee-tab-width preferred-javascript-indent-level))

(when (fboundp 'coffee-mode)
  (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode)))

;; ---------------------------------------------------------------------------
;; Run and interact with an inferior JS via js-comint.el
;; ---------------------------------------------------------------------------

(when (maybe-require-package 'js-comint)
  (setq inferior-js-program-command "js")

  (defvar inferior-js-minor-mode-map (make-sparse-keymap))
  (define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
  (define-key inferior-js-minor-mode-map "\C-\M-x" 'js-send-last-sexp-and-go)
  (define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)
  (define-key inferior-js-minor-mode-map "\C-c\C-b" 'js-send-buffer-and-go)
  (define-key inferior-js-minor-mode-map "\C-cl" 'js-load-file-and-go)

  (define-minor-mode inferior-js-keys-mode
    "Bindings for communicating with an inferior js interpreter."
    nil " InfJS" inferior-js-minor-mode-map)

  (dolist (hook '(js2-mode-hook js-mode-hook))
    (add-hook hook 'inferior-js-keys-mode)))

;; ---------------------------------------------------------------------------
;; Alternatively, use skewer-mode
;; ---------------------------------------------------------------------------

(when (maybe-require-package 'skewer-mode)
  (after-load 'skewer-mode
    (add-hook 'skewer-mode-hook
              (lambda () (inferior-js-keys-mode -1)))))


;; moz-repl
(require-package 'moz)
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

;; js2 enhance
(defun js2-remap-array ()
  "Replace array expression with real value, eg. arr[1] into 123."
  (interactive)
  (let*
      (
       (node (js2-node-at-point))
       (current-start (js2-node-abs-pos node))
       (current-pos current-start)
       (offset 0)
       (elems (js2-array-to-list node))
       (pattern (concat (js2-name-node-name node) "\\[\\(\[0-9\]+\\)\\]"))
       )
    (save-excursion
      (mapc (lambda (x)
              (when (not (= x current-start))
                (if (> x current-pos)
                    (goto-char (+ offset x))
                    (goto-char x))                
                (setq current-pos (point))
                (when (search-forward-regexp pattern nil t)
                  (let* ((content (match-string-no-properties 0))
                         (index (string-to-number (match-string-no-properties 1)))
                         (replacement (nth index elems))
                         )
                    (goto-char current-pos)
                    (setq offset (- (length content) (length replacement)))
                    (delete-region current-pos
                                   (+ current-pos (length content)))
;                    (print `(,index  ,replacement ,content))
                    (insert replacement)))))
            (js2r--local-var-positions node)))))


(defun js2-array-to-list (&optional node)
  "convert array to list"
  (let ((node (or node (js2-node-at-point)))
        elems)
    (unless (js2-var-init-node-p (js2-node-parent node))
      (error "invalid node"))
    (setq elems (js2-array-node-elems
                 (js2-var-init-node-initializer
                  (js2-node-parent node))))
    (mapcar (lambda (x) (js2-node-string x)) elems)))

;(decode-coding-string "keywork" 'utf-8)
(defun js2-unescape (b)
  "Convert hex or unicode encoded string B to normal string."
  (interactive "bselect buffer ")
  (let*
      ((hex-pattern "\\\\x\\(\\w+\\)")
       (unicode-pattern "\\\\u\\(\\w+\\)")
       (process 
        #'(lambda (p)
            (with-current-buffer b
              (save-excursion
                (goto-char (point-min))
                (while (search-forward-regexp p nil t)
                  (replace-match 
                   (format "%c" (string-to-number (match-string 1) 16))
                   nil t))
                )))))

    (funcall process hex-pattern)
    (funcall process unicode-pattern)))

(defun js2r-rename-var (replacement)
  "Renames the variable REPLACEMENT on point and all occurrences in its lexical scope."
  (interactive "sInput the replacement: ")
;  (js2r--guard)
  (let* ((current-node (js2r--local-name-node-at-point))
         (len (js2-node-len current-node))
         (offset (- (length replacement) len))
         (current-start (js2-node-abs-pos current-node))
         (current-end (+ current-start len)))

    (goto-char current-start)
    (delete-region (point) (+ (point) len))
    (insert replacement)

    (save-excursion
      (mapc (lambda (beg)
              (when (not (= beg current-start))
                (if (> beg current-start)
                    (goto-char (+ offset beg))
                    (goto-char beg))
                (delete-region (point) (+ (point) len))
                (insert replacement)
                ))
            (js2r--local-var-positions current-node)))))

(provide 'init-javascript)

;;; init-javascript.el ends here
