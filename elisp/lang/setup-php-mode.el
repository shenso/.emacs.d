(use-package php-mode
  :ensure t)

(use-package company-php
  :ensure t
  :hook
  (php-mode . (lambda ()
		(company-mode t)
		(add-to-list 'company-backends
			     'company-ac-php-backend))))

(use-package geben
  :ensure t)
