;;; theme-config.el --- theme

;;; Commentary:
;; 


;;; Code:

(use-package doom-themes
  :ensure t
  :config

  (let ((theme 'doom-molokai))
    (unless (custom-theme-p theme)
      (load-theme theme))
    (setq-default custom-enabled-themes `(,theme))
    (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

  ;; doom related
  (doom-themes-visual-bell-config)
  
  ;; settings
  (setq-default initial-scratch-message
		";; Welcome home, good hunter. May Emacs helps you!\n\n")
  ;;----------------------------------------------------------------------------
  ;; Suppress GUI features
  ;;----------------------------------------------------------------------------
  (setq use-file-dialog nil)
  (setq use-dialog-box nil)

  ;;----------------------------------------------------------------------------
  ;; Show a marker in the left fringe for lines not in the buffer
  ;;----------------------------------------------------------------------------
  (setq indicate-empty-lines t)


  ;;----------------------------------------------------------------------------
  ;; Window size and features
  ;;----------------------------------------------------------------------------
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode nil))

  (let ((no-border '(internal-border-width . 0)))
    (add-to-list 'default-frame-alist no-border)
    (add-to-list 'initial-frame-alist no-border))

  ;; frame title
  (setq frame-title-format
	'((:eval (if (buffer-file-name)
		     (abbreviate-file-name (buffer-file-name))
		   "%b"))))
  :config
  (defun sanityinc/adjust-opacity (frame incr)
    "Adjust the background opacity of FRAME by increment INCR."
    (unless (display-graphic-p frame)
      (error "Cannot adjust opacity of this frame"))
    (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
	   ;; The 'alpha frame param became a pair at some point in
	   ;; emacs 24.x, e.g. (100 100)
	   (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
	   (newalpha (+ incr oldalpha)))
      (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
	(modify-frame-parameters frame (list (cons 'alpha newalpha))))))

  
  (bind-key "M-C-8" (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
  (bind-key "M-C-9" (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
  (bind-key "M-C-0" (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;model line;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  %b -- print buffer name.      %f -- print visited file name.
;;  %F -- print frame name.
;;  %* -- print %, * or hyphen.   %+ -- print *, % or hyphen.
;;        %& is like %*, but ignore read-only-ness.
;;        % means buffer is read-only and * means it is modified.
;;        For a modified read-only buffer, %* gives % and %+ gives *.
;;  %s -- print process status.   %l -- print the current line number.
;;  %c -- print the current column number (this makes editing slower).
;;        To make the column number update correctly in all cases,
;;        `column-number-mode' must be non-nil.
;;  %i -- print the size of the buffer.
;;  %I -- like %i, but use k, M, G, etc., to abbreviate.
;;  %p -- print percent of buffer above top of window, or Top, Bot or All.
;;  %P -- print percent of buffer above bottom of window, perhaps plus Top,
;;        or print Bottom or All.
;;  %n -- print Narrow if appropriate.
;;  %t -- visited file is text or binary (if OS supports this distinction).
;;  %z -- print mnemonics of keyboard, terminal, and buffer coding systems.
;;  %Z -- like %z, but including the end-of-line format.
;;  %e -- print error message about full memory.
;;  %@ -- print @ or hyphen.  @ means that default-directory is on a
;;        remote machine.
;;  %[ -- print one [ for each recursive editing level.  %] similar.
;;  %% -- print %.   %- -- print infinitely many dashes.
(use-package nyan-mode
  :if (display-graphic-p)
  :ensure t
  :init
  (nyan-mode))

(use-package rich-minority
  :ensure t
  :init
  (rich-minority-mode 1))

(setq display-time-24hr-format t)
(display-time-mode)

(column-number-mode)
(size-indication-mode)

(use-package smart-mode-line
  :ensure t
  :init

  (setq sml/theme 'respectful)
  (setq sml/mode-width 0)
  (setq sml/name-width 20)
;  (setf rm-blacklist "")
  ;;   :config

  (sml/setup))

(provide 'theme-config)

;;; theme-config.el ends here
