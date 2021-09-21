(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package docker-compose-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("docker-compose.yml" . docker-compose-mode)))

(use-package yaml-mode
  :ensure t)
