;; company-mode (auto-complete)
(use-package company
  :ensure t
  :config
  (setq-default company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2))

;; flycheck (syntax checker)
(use-package flycheck
  :ensure t)
