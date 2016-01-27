;;; init-org.el --- org mode

;;; Commentary:
;; 


;;; Code:
(require-package 'org)
;(require-package 'ox-reveal)
;(require-package 'org-trello)

;; Key bindings
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)


;; Settings
(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)

(setq org-enforce-todo-dependencies t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(s)" "|" "DONE(d!/!) FAILED(f)" );general tasks
       (sequence "CODING" "TESING" "REFACTOR(!/@)"  "|" "REVIEW(/@)");coding
       (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT" "DELEGATED(@/)" "|" "CANCELLED(c@/!)")));task states


(setq org-tag-alist '((:startgroup . nil)
		      ("@work" . ?w) 
		      ("@home" . ?h)
		      ("@gf's home" . ?t)
		      (:endgroup . nil)
		      (:startgroup . nil)
		      ("laptop" . ?l)
		      ("pc" . ?p)
		      ("phone" .?P)
		      (:endgroup . nil)
		      (:startgroup . nil)
		      ("dev" . nil)
		      ("personal" . nil)
		      ("blog" . nil)
		      ("thought" .nil)
                      ("twit" . nil)
		      (:endgroup . nil)
		      ))
;archive file
(setq org-archive-location "%s.archive::")

(after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((R . t)
     (ditaa . t)
     (dot . t)
     (plantuml . t)
     (clojure . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (sql . nil)
     (sqlite . t))))

(setq org-confirm-babel-evaluate nil)
(setq org-startup-with-inline-images t);; auto show images
(setq org-plantuml-jar-path
      (expand-file-name "~/bin/plantuml.jar"))

;;; blogging
;; deprecated... 

(provide 'init-org)

;;; init-org.el ends here
