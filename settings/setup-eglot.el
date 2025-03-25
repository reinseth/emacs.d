(use-package eglot
  :bind
  (:map eglot-mode-map
        ("s-l r" . eglot-rename)
        ("s-l a" . eglot-code-actions)
        ("s-l o" . eglot-code-action-organize-imports)
        ("M-s M-o" . consult-eglot-symbols))
  :config
  ;; The following is necessary to get flymake-eslint to work with eglot
  ;; See https://github.com/joaotavora/eglot/issues/268
  (setq eglot-stay-out-of '(flymake))
  (add-hook 'eglot--managed-mode-hook (lambda () (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t))))

(use-package consult-eglot
  :ensure t)

(provide 'setup-eglot)
