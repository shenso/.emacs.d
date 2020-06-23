;; company-mode (auto-complete)
(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2))

;; flycheck (syntax checker)
(use-package flycheck)
