(use-package flycheck
  :ensure t
  :config
  ;; c/c++ linting
  (use-package flycheck-irony
    :ensure t
    :after irony
    :hook
    (((c-mode c++-mode) . flycheck-mode)
     (flycheck-mode . flycheck-irony-setup))))
