;; Chat client for LLMs
;; For ChatGPT, add the api key to ~/.authinfo like so:
;;
;;    machine api.openai.com login apikey password THE_ACTUAL_API_KEY
;;
(use-package gptel
  :ensure t)

(defun translate-json-value-at-point ()
  "Translate the JSON string value at point using GPT and replace it in place."
  (interactive)
  (let (original-text translated-text)
    (save-excursion
      ;; Check if the point is inside a JSON string
      (when (re-search-backward "\"\\(.*?\\)\"" nil t)
        (setq original-text (match-string 1)) ;; Get the text inside quotes
        ;; Send the text to GPT for translation
        (setq translated-text
              (gptel-send
               (concat "Translate this text into the desired language: " original-text)))
        ;; Prompt user for confirmation
        (when (y-or-n-p (format "Replace '%s' with translation '%s'? " original-text translated-text))
          ;; Replace the original text with the translated text
          (replace-match (concat "\"" translated-text "\"") t t))))))

(provide 'setup-gptel)
