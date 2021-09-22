;; c/c++/java
(use-package cc-mode
  :ensure t
  :pin manual
  :init
  (setq-default c-basic-offset 4
		c-default-style '((other . "k&r")))
  :config
  ;; c/c++ minor mode
  (use-package irony
    :ensure t
    :init
    (setq w32-pipe-read-delay 0))
  :hook
  (cc-mode . (lambda ()
	       (setq indent-tabs-mode nil
		     tab-width 4))))

;; python
(use-package python-mode
  :ensure t
  :hook (python-mode . flycheck-mode))

;; typescript
(use-package typescript-mode
  :ensure t
  :config
  (use-package tide
    :ensure t
    :after (flycheck company)
    :init
    :hook ((typescript-mode . tide-setup)
	   (typescript-mode . tide-hl-identifier-mode))))

(use-package web-mode
  :ensure t
  :after php-mode
  :init
  (setq web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 4
	web-mode-code-indent-offset 4)
  ;;(setq web-mode-ac-sources-alist
	;;'(("php" . (company-ac-php-backend))))
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  ;;(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  ;; no php until company issues resolved
  :hook
  (web-mode . (lambda ()
		(setq indent-tabs-mode nil
		      tab-width 4))))

;; php
(use-package php-mode
  :ensure t)

;; lua
(use-package lua-mode
  :ensure t
  :config
  (setq lua-indent-level 4))

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
