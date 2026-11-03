;;; project-config.el --- Project management  -*- lexical-binding: t; -*-

;;; Commentary:
;; The built-in project.el with project-tool.el: any folder can be a
;; project, and Emacs started without files shows the favorite and
;; recent projects.  F7 compiles and F9 opens Eshell in the project root;
;; the project commands show folders in `dired-sidebar' rather than Dired.

;;; Code:
(require 'seq)
(require 'project-tool)

(defvar frei/state-directory)

(defun frei/project-eshell ()
  "Open an Eshell in the current project, or here outside a project.
A numeric prefix argument selects a numbered session."
  (interactive)
  (call-interactively (if (project-current) #'project-eshell #'eshell)))

(declare-function dired-sidebar-showing-sidebar-p "dired-sidebar")
(declare-function dired-sidebar-buffer "dired-sidebar")

(defun frei/project-sidebar ()
  "Show the current project in `dired-sidebar' and select the sidebar.
Unlike \\[dired-sidebar-toggle-sidebar], an open sidebar switches to
the project instead of closing, which suits switching projects."
  (interactive)
  (require 'dired-sidebar)
  (let ((root (expand-file-name (project-root (project-current t)))))
    (when (dired-sidebar-showing-sidebar-p)
      (dired-sidebar-hide-sidebar))
    (dired-sidebar-toggle-sidebar root)
    (select-window (get-buffer-window (dired-sidebar-buffer)))))

(add-to-list 'display-buffer-alist
             '("\\`\\*.*eshell\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 0.25)))

(add-hook 'emacs-startup-hook #'frei/project-list-at-startup)
(add-hook 'after-save-hook #'frei/project-note-save)
(add-hook 'kill-emacs-hook #'frei/project-save-times)

(use-package project
  :ensure nil
  :bind
  (([f7] . project-compile)
   ([f9] . frei/project-eshell)
   :map project-prefix-map
   ("a" . frei/project-add-directory)
   ("b" . consult-project-buffer)
   ("D" . frei/project-sidebar)
   ("l" . frei/project-list))
  :custom
  (project-list-file
   (expand-file-name "projects.eld" frei/state-directory))
  ;; The menu after C-x p p.  Magit appears only when it is installed.
  (project-switch-commands
   (seq-filter (lambda (entry) (fboundp (car entry)))
               '((project-find-file "Find file" ?f)
                 (consult-ripgrep "Ripgrep" ?r)
                 (consult-project-buffer "Buffer" ?b)
                 (frei/project-sidebar "Sidebar" ?d)
                 (magit-project-status "Magit" ?m)
                 (project-eshell "Eshell" ?e)
                 (project-compile "Compile" ?c)
                 (project-any-command "Other" ?o))))
  :config
  ;; Added folders and favorites before version control, markers after.
  (add-hook 'project-find-functions #'frei/project-try-configured -50)
  (add-hook 'project-find-functions #'frei/project-try-markers 50)
  (frei/project-remember-configured))

(provide 'project-config)

;;; project-config.el ends here
