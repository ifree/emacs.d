;;; window-config.el --- window management

;;; Commentary:
;; 


;;; Code:
;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
(winner-mode 1)


(setq lexical-binding t)
;; Make "C-x o" prompt for a target window when there are more than 2
(use-package switch-window
  :ensure t
  :init
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message t)
  
  (setq-default switch-window-shortcut-style 'alphabet)
  (setq-default switch-window-timeout nil)
  (setq lexical-binding t)
  ;;----------------------------------------------------------------------------
  ;; When splitting window, show (other-buffer) in the new window
  ;;----------------------------------------------------------------------------
  ;; since I rarely use lower version of emacs, lexcial binding is the best option

  (defun split-window-func-with-other-buffer (split-function)
    (let ((lexical-binding t)
	  (s-f split-function))
      (lambda (&optional arg)
	"Split this window and switch to the new window unless ARG is provided."
	(interactive "P")
	(funcall s-f)
	(let ((target-window (next-window)))
	  (set-window-buffer target-window (other-buffer))
	  (unless arg
	    (select-window target-window))))))

  ;;----------------------------------------------------------------------------
  ;; Rearrange split windows
  ;;----------------------------------------------------------------------------
  (defun split-window-horizontally-instead ()
    (interactive)
    (save-excursion
      (delete-other-windows)
      (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

  (defun split-window-vertically-instead ()
    (interactive)
    (save-excursion
      (delete-other-windows)
      (funcall (split-window-func-with-other-buffer 'split-window-vertically))))
  
  (bind-key "C-x 2"  (split-window-func-with-other-buffer 'split-window-vertically))
  (bind-key "C-x 3"  (split-window-func-with-other-buffer 'split-window-horizontally))
  
  :bind (("C-x o" . switch-window)
	 ("C-x |" . split-window-horizontally-instead)
	 ("C-x _" . split-window-vertically-instead)))


(provide 'window-config)

;;; window-config.el ends here
