;; TODO can we bind flymake to next-error/previous-error using the next-error-function?
;; This will allow us to navigate errors with M-g n and M-g p
(use-package flymake
  :hook ((prog-mode . flymake-mode)
         (emacs-lisp-mode . (lambda () (flymake-mode -1)))))

(provide 'setup-flymake)
