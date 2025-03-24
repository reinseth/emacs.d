(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch)
         ("C-x p s" . magit-save-repository-buffers))
  :config
  (setq magit-section-initial-visibility-alist '((untracked . show)
                                                 (unstaged . show)
                                                 (unpushed . show)
                                                 (unpulled . show)
                                                 (stashes . hide)))
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1)
  (setq magit-bury-buffer-function 'magit-restore-window-configuration)
  (setq magit-diff-refine-hunk t)
  (setq magit-no-confirm '(stage-all-changes unstage-all-changes))
  (add-hook 'git-commit-setup-hook 'magit-insert-jira-task))

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  :config
  (setq diff-hl-flydiff-mode t)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :bind (("s-[" . diff-hl-previous-hunk)
         ("s-]" . diff-hl-next-hunk)
         ("s-h" . diff-hl-show-hunk)))

(use-package vc-msg
  :ensure t
  :bind (("s-H" . vc-msg-show)))

(use-package git-timemachine
  :ensure t
  :bind (("C-x v t" . git-timemachine)))

(defvar magit-jira-task nil)

(defun magit-insert-jira-task ()
  (when magit-jira-task
    (unless (save-excursion
              (goto-char (point-min))
              (search-forward magit-jira-task nil t))
      (save-excursion
        (insert (concat "\n\n" magit-jira-task "\n"))))))

(defun magit-set-jira-task ()
  (interactive)
  (setq magit-jira-task (read-string "Jira task: " magit-jira-task)))

(provide 'setup-git)
