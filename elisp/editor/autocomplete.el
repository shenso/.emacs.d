(use-package company
  :ensure t
  :init
  (setq-default company-idle-delay 0.1
		company-minimum-prefix-length 2)
  :config
  ;; c/c++ autocompletion
  (use-package company-irony
    :ensure t
    :after irony
    :config
    (add-to-list 'company-backends 'company-irony)
    :hook
    (((c-mode c++-mode) . (lambda ()
			    (setq company-idle-delay 0)))
     ((c-mode c++-mode) . company-mode)))
  ;; python autocompletion
  (use-package company-jedi
    :ensure t
    :after company
    :init
    (setq python-shell-interpreter "/usr/bin/env python3")
    :config
    (add-to-list 'company-backends 'company-jedi)
    :hook
    (python-mode . (lambda ()
		     (company-mode t))))
  (use-package company-php
    :ensure t
    :after php-mode
    :hook
    (php-mode . (lambda ()
		  (company-mode t)
		  (add-to-list 'company-backends
			       'company-ac-php-backend)))))
