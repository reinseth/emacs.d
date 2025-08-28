;; Chat client for LLMs
;; For ChatGPT, add the api key to ~/.authinfo like so:
;;
;;    machine api.openai.com login apikey password THE_ACTUAL_API_KEY
;;
(use-package gptel
  :ensure t
  :custom
  (gptel-default-mode 'org-mode))

(provide 'setup-gptel)
