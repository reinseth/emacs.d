(use-package scratch
  :ensure t)

(defun open-scratch-buffer ()
  (interactive)
  (pop-to-buffer (get-scratch-buffer-create)))

(defun new-scratch-buffer ()
  (interactive)
  (let ((current-prefix-arg 1))
    (call-interactively 'scratch)))

(provide 'setup-scratch)
