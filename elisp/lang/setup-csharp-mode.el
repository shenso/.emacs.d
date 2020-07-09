(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook 'setup-csharp-indentation))
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

(defun setup-csharp-indentation ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4))
