(use-package hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :bind (("M--" . hs-hide-block)
         ("M-=" . hs-show-block)
         ("M-+" . hs-show-all)
         ("M-_" . hs-hide-all)))

(provide 'setup-hideshow-mode)
