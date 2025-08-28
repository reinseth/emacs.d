(use-package eldoc-box
  :ensure t)

(add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t)

(provide 'setup-eldoc)
