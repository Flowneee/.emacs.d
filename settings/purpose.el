;;;

(require 'window-purpose)
(purpose-mode)

;;; Edit modes
(add-to-list 'purpose-user-mode-purposes '(python-mode . edit))
(add-to-list 'purpose-user-mode-purposes '(c-mode . edit))
(add-to-list 'purpose-user-mode-purposes '(c++-mode . edit))
(add-to-list 'purpose-user-mode-purposes '(web-mode . edit))
(add-to-list 'purpose-user-mode-purposes '(shell-mode . edit))
(add-to-list 'purpose-user-mode-purposes '(makefile-mode . edit))
(add-to-list 'purpose-user-mode-purposes '(lisp-mode . edit))

;;; Term modes
(add-to-list 'purpose-user-mode-purposes '(term-mode . term))
(add-to-list 'purpose-user-mode-purposes '(eshell-mode . term))
(add-to-list 'purpose-user-mode-purposes '(shell-mode . term))

;;; Dir browser modes
(add-to-list 'purpose-user-mode-purposes '(speedbar-mode . dir-browsers))

(purpose-compile-user-configuration)

;;; purpose.el ends here
