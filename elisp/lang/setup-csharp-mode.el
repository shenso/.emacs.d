(use-package csharp-mode)
(use-package omnisharp
  :requires (csharp-mode company)
  :after (csharp-mode company)
  :config
  ;; company stuff
  (add-to-list 'company-backends 'company-omnisharp)
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  ;; autocomplete
  (add-hook 'csharp-mode-hook 'company-mode)
  ;; flycheck
  (add-hook 'csharp-mode-hook 'flycheck-mode))
