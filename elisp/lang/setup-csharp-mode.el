(use-package csharp-mode)
(use-package omnisharp
  :after (company)
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook 'company-mode))
