;; Apheleia: autoformat on save with prettier (and others)
(use-package apheleia
  :ensure t
  :diminish apheleia-global-mode
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath)))

(provide 'setup-apheleia)
