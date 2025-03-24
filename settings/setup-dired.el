(use-package dired
  :config
  (defun my-dired-move-file-to-nonexistent-directory (file dest)
    "Move FILE to DEST, creating directories if needed."
    (let ((dest-dir (file-name-directory dest)))
      (unless (file-exists-p dest-dir)
        (make-directory dest-dir t))
      (rename-file file dest)))

  (defun my-dired-do-rename ()
    "In Dired, rename the current file, creating target directories if necessary."
    (interactive)
    (let* ((file (dired-get-file-for-visit))
           (dest (read-file-name "Move to: " (dired-dwim-target-directory))))
      (my-dired-move-file-to-nonexistent-directory file dest)
      (revert-buffer)))

  ;; Bind the custom rename function to a key in dired-mode
  (define-key dired-mode-map (kbd "R") 'my-dired-do-rename))

(provide 'setup-dired)
