(use-package python-mode
  :ensure t
  :config
  (use-package company-jedi
    :ensure t
    :after company
    :init
    (setq python-shell-interpreter "/usr/bin/python3")
    :config
    (add-to-list 'company-backends 'company-jedi)
    :hook
    (python-mode . (lambda ()
		     (company-mode t))))
  :hook
  (python-mode . flycheck-mode))
