(use-package paredit
  :ensure t
  :hook ((clojure-mode . paredit-mode)
         (cider-repl-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-data-mode . paredit-mode))
  :bind
  (:map paredit-mode-map
        ("C-w" . paredit-kill-region-or-backward-word)
        ("M-k" . paredit-kill)
        ("C-S-k" . paredit-kill)
        ("C-M-{" . paredit-wrap-curly)
        ("C-c M-{" . paredit-wrap-curly)
        ("M-[" . paredit-wrap-square)
        ("C-c M-[" . paredit-wrap-square)
        ("C-c d" . paredit-duplicate-after-point)

        ;; Rebind splice - gets in the way of search
        ("M-s" . nil)
        ("s-s" . paredit-splice-sexp-killing-backward)

        ;; Rebind raise - gets in the way of cursor positioning
        ("M-r" . nil)
        ("s-r" . paredit-raise-sexp)

        ;; Unbind convolute - gets in the way of xref-find-references
        ("M-?" . nil)

        ;; Unbind paredit new line - gets in the way of my expand-region binding
        ("C-j" . nil)

        ("M-J" . my/paredit-join-sexps)))

(use-package paredit-menu
  :ensure t
  :after paredit)

;; Taken from https://github.com/magnars/emacsd-reboot
(defun paredit-kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (paredit-backward-kill-word)))

;; Duplicate sexp - source: https://github.com/cursive-ide/cursive/issues/722
(defun paredit-duplicate-after-point ()
  "Duplicates the content of the line that is after the point."
  (interactive)
  ;; skips to the next sexp
  (while (looking-at " ")
    (forward-char))
  (set-mark-command nil)
  ;; while we find sexps we move forward on the line
  (while (and (<= (point) (car (bounds-of-thing-at-point 'sexp)))
              (not (= (point) (line-end-position))))
    (forward-sexp)
    (while (looking-at " ")
      (forward-char)))
  (kill-ring-save (mark) (point))
  ;; go to the next line and copy the sexprs we encountered
  (paredit-newline)
  (set-mark-command nil)
  (yank)
  (exchange-point-and-mark))

(defun my/paredit-join-sexps ()
  "Calls the standard paredit-join-sexps function which joins lists and strings.
If that fails, join the current line with the next line."
  (interactive)
  (condition-case err
      (paredit-join-sexps)
    (error (crux-top-join-line))))

(provide 'setup-paredit)
