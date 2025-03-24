(use-package breadcrumb
  :ensure t
  :config
  ;; I use narrow splits, but I still want the symbol names printed in full
  (setq breadcrumb-imenu-max-length 0.9)
  ;; Hack to remove the filename crumb
  (fset 'breadcrumb--project-crumbs-1 #'ignore))

(provide 'setup-breadcrumb-mode)
