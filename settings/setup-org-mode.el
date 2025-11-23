(use-package org
  :bind (:map org-mode-map
              ("C-j" . nil))
  :config
  (setq org-capture-templates
        '(("d" "Dagbok" entry (file+datetree "~/org/dagbok.org")
           "* %? \n %i "
           :jump-to-captured t)
          ("b" "Bøker" entry (file+headline "~/org/lister.org" "Bøker")
           "* %?\n %i\n")
          ("l" "Blogger / Videoer" entry (file+headline "~/org/lister.org" "Blogger / Videoer")
           "* %?\n %i\n")
          ("f" "Filmer" entry (file+headline "~/org/lister.org" "Filmer"))
          ("s" "Serier" entry (file+headline "~/org/lister.org" "Serier"))
          ("p" "Programmering" entry (file+headline "~/org/lister.org" "Programmering"))
          ("t" "Todo" entry (file "~/org/todo.org")
           "* TODO %?"
           :prepend t)))
  (setq org-agenda-files "~/org/.agenda_files")
  (setq org-list-allow-alphabetical t))

(provide 'setup-org-mode)
