;; Use plists for deserialization of json from the lsp servers
;; See https://emacs-lsp.github.io/lsp-mode/page/performance/
(setenv "LSP_USE_PLISTS" "true")

;; From Emacs Bedrock: Startup speed, annoyance suppression
(setq gc-cons-threshold 10000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq inhibit-startup-echo-area-message (user-login-name))
