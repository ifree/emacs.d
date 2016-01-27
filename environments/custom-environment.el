;;; custom-environment.el --- Customize working environments definitions

;;; Commentary:
;; Tool for emacs module mangement.

;;; Code:
;;
(eval-when-compile (require 'subr-x))

(defvar custom-environment--prefix "my-env-")

(defun custom-environment--load-modules (env-sym)
  "Load ENV-SYM's modules."
  (let* ((props (get env-sym :props))
         (based-on (intern-soft
                    (concat
                     custom-environment--prefix
                     (plist-get props :based-on))))
         (modules (if based-on
                      (append
		       (custom-environment--load-modules based-on)
		       (plist-get props :modules))
                    (plist-get props :modules)))
         (exclude-modules (plist-get props :exclude-modules))
         (ret '()))
    (dolist (m modules)
      (unless (member m exclude-modules)
        (add-to-list 'ret m t))) ;append to keep load order
    ret))

;;;###autoload
(defun custom-environment-define (name &rest args)
  "Define environment NAME with properties ARGS.
Your environment contains `:modules' `:exclude-modules' and an
optional base module by specify a `:based-on' property.  eg:
    (custom-environment-define
     \"common\"
     :based-on \"base\"
     :modules '(lisp
                blogging
                company
                cpp)
     :exclude-modules '(android clojure))"
  (let ((sym (intern (concat custom-environment--prefix name))))
    (put sym :props args)))

;;;###autoload
(defun custom-environment-load (name)
  "Load environment by NAME."
  (if-let ((sym (intern-soft (concat custom-environment--prefix name)))
           (modules (custom-environment--load-modules sym)))
      (dolist (m modules)
        (if-let ((module-sym (intern (concat "init-" (symbol-name m)))))
            (require module-sym)
          (error "Symbol init-%s doesn't exists" (symbol-name m))))
    (error "Environment doesn't exists!")))


(provide 'custom-environment)

;;; custom-environment.el ends here
