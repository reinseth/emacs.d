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

;; Source: https://stackoverflow.com/a/32002122
(defun my/isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (goto-char (region-beginning))
      (deactivate-mark)
      (isearch-update)
      (isearch-yank-string region))))

(add-hook 'isearch-mode-hook #'my/isearch-with-region)

(provide 'setup-search-replace)
