;;; init-mode-line.el --- Mode line settings

;;; Commentary:
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


;;; Code:

(if (display-graphic-p)
    (progn
      (require-package 'nyan-mode)
      (nyan-mode)))

(set-face-attribute 'mode-line nil
		    :foreground "gray60" :background "gray20"
		    :inverse-video nil
		    :box '(:line-width 6 :color "gray20" :style nil))

(setq-default mode-line-format
	      (list
	       ;; the buffer name; the file name as a tool tip
	       '(:eval (propertize "%b " 'face 'font-lock-string-face
				   'help-echo (buffer-file-name)))
	       "["
	       (propertize "%Z" 'face 'font-lock-doc-face)
	       "]"
	       ;; line and column
	       "(" ;; '%02' to set to 2 chars at least; prevents flickering
	       (propertize "%02l" 'face 'font-lock-type-face) ","
	       (propertize "%02c" 'face 'font-lock-type-face) 
	       ") "

	       ;; relative position, size of file
	       "["
	       (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
	       "/"
	       (propertize "%I" 'face 'font-lock-constant-face) ;; size
	       "] "
	       
					;	       "["
					;	       '(:eval (propertize (symbol-name buffer-file-coding-system) 'face 'font-lock-type-face
					;				   'help-echo buffer-file-coding-system))
					;	       "] "

	       ;; the current major mode for the buffer.
	       "["

	       '(:eval (propertize "%m" 'face 'font-lock-string-face
				   'help-echo buffer-file-coding-system))
	       "] "


	       "[" ;; insert vs overwrite mode, input-method in a tooltip
	       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
				   'face 'font-lock-preprocessor-face
				   'help-echo (concat "Buffer is in "
						      (if overwrite-mode "overwrite" "insert") " mode")))

	       ;; was this buffer modified since the last save?
	       '(:eval (when (buffer-modified-p)
			 (concat ","  (propertize "Mod"
						  'face 'font-lock-warning-face
						  'help-echo "Buffer has been modified"))))

	       ;; is this buffer read-only?
	       '(:eval (when buffer-read-only
			 (concat ","  (propertize "RO"
						  'face 'font-lock-builtin-face
						  'help-echo "Buffer is read-only"))))  
	       "] "

	       ;; add the time, with the date and the emacs uptime in the tooltip
	       '(:eval (propertize (format-time-string "%H:%M")
				   'help-echo
				   (concat (format-time-string "%c; ")
					   (emacs-uptime "Uptime:%hh"))))
	       " ["
               '(:eval minor-mode-alist)  ;; list of minor modes
	       "] "

	       '(:eval (list (if (display-graphic-p) (nyan-create) ":>")))
	       ;; i don't want to see minor-modes; but if you want, uncomment this:

	       ;;"%-" ;; fill with '-'
	       ))

(font-lock-add-keywords
 nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
	1 font-lock-warning-face t)))

(provide 'init-mode-line)

;;; init-mode-line.el ends here
