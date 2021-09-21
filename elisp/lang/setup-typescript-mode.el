(use-package typescript-mode
  :ensure t
  :config
  (use-package tide
    :ensure t
    :after (flycheck company)
    :init
    :hook ((typescript-mode . tide-setup)
	   (typescript-mode . tide-hl-identifier-mode))))
