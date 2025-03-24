(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package rg
  :ensure t
  :bind ("M-s s" . gar/rg-project)
  :config
  (rg-define-search gar/rg-project
    :files "everything"
    :dir project))

;; Occur buffer navigation
(define-key occur-mode-map (kbd "n") 'occur-next)
(define-key occur-mode-map (kbd "p") 'occur-prev)

(provide 'setup-search-replace)
