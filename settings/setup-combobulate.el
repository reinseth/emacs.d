(use-package combobulate
  :preface
  (setq combobulate-key-prefix "C-c o")
  :bind (nil
         :map combobulate-key-map
         ("s-s" . combobulate-splice-up)
         ("s-r" . combobulate-splice-self))
  :hook
  ((js-ts-mode . combobulate-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode))
  ;; TODO use straight.el
  :load-path ("~/Tools/combobulate"))

(provide 'setup-combobulate)
