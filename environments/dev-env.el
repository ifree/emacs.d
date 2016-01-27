;;; dev-env.el --- Coding environment

;;; Commentary:
;; 


;;; Code:

(custom-environment-define
 "dev"
 :based-on "common"
 :modules '(
	    flycheck
	    projectile
	    cpp
	    paredit
	    ecb
	    lisp
	    clojure
	    ;haskell
	    lua
	    javascript
	    org
	    git
	    yasnippet
	    dev-tools
            ))

(provide 'dev-env)

;;; dev-env.el ends here
