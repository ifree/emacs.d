;;; init-ecb.el --- code browser

;;; Commentary:
;; 


;;; Code:
(require-package 'ecb)

;(require 'xcscope)

(require 'ecb)

;(semanticdb-enable-gnu-global-databases 'actionscript-mode)
(setq ecb-process-non-semantic-files t)


;;; layout
;; (defecb-window-dedicator-to-ecb-buffer ecb-set-cscope-buffer "*ECB cscope-buf*" nil
;;   (switch-to-buffer "*cscope*"))

;; (ecb-layout-define "ecb-cscope-layout" left
;; "
;; This function creates the following layout:

;;    -------------------------------------------------------
;;    |              |                                      |
;;    |  Directories |                                      |
;;    |              |                                      |
;;    |--------------|                                      |
;;    |              |                                      |
;;    |  Sources     |                                      |
;;    |              |                                      |
;;    |--------------|                 Edit                 |
;;    |              |                                      |
;;    |  Methods     |                                      |
;;    |              |                                      |
;;    |              |                                      |
;;    |--------------|                                      |
;;    |  Cscope      |                                      |
;;    |              |                                      |
;;    -------------------------------------------------------
;;    |                                                     |
;;    |                    Compilation                      |
;;    |                                                     |
;;    -------------------------------------------------------
;; "  
;; (ecb-set-directories-buffer)
;;   (ecb-split-ver 0.3)
;;   (ecb-set-sources-buffer)
;;   (ecb-split-ver 0.35)
;;   (ecb-set-methods-buffer)
;;   (ecb-split-ver 0.65)
;;   (ecb-set-cscope-buffer))

;(setq ecb-layout-name "ecb-cscope-layout")
;(setq ecb-layout-name "left8")

;;; callbacks
(tree-buffer-defpopup-command ecb-open-external
  "open file or dir in external app"
  (let (
        (dir-or-file (tree-node->data node)))
    (browse-url (concat "file://" dir-or-file))))



(setq ecb-sources-menu-user-extension
      (append
       ecb-sources-menu-user-extension
       '((ecb-open-external "Open external")
         ("---"))))

(setq ecb-directories-menu-user-extension
      (append
       ecb-directories-menu-user-extension
       '((ecb-open-external "Open external")
         ("---"))))

(provide 'init-ecb)

;;; init-ecb.el ends here
