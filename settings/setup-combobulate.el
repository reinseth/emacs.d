(use-package combobulate
  :straight (combobulate :host github
                         :repo "mickeynp/combobulate")
  :preface
  (setq combobulate-key-prefix "C-c o")
  :bind (nil
         :map combobulate-key-map
         ("s-s" . combobulate-splice-up)
         ("s-r" . combobulate-splice-self))
  :hook
  ;; is otherwise activated in setup-web.el
  ((yaml-ts-mode . combobulate-mode))
  )

(provide 'setup-combobulate)
