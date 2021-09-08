(defun setup-php-indentation ()
  (setq indent-tabs-mode t)
  (setq tab-width 4))

(use-package php-mode
  :ensure t
  :config
  (add-hook 'php-mode-hook 'setup-php-indentation))

(defun setup-php-company ()
  (company-mode t)
  (add-to-list 'company-backends 'company-ac-php-backend))

(use-package company-php
  :ensure t
  :config
  (add-hook 'php-mode-hook 'setup-php-company))

(use-package geben
  :ensure t)
