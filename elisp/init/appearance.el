(require 'packaging)

;; disable toolbar/gui stuff
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; line numbers
(use-package nlinum
  :ensure t
  :init
  (setq nlinum-format " %d")
  (setq nlinum-relative-redisplay-delay 0)
  :config
  (use-package nlinum-relative
    :ensure t
    :hook ((prog-mode text-mode) . nlinum-relative-mode)))

;; on first install do M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

;; theme
(use-package doom-themes
  :ensure t
  :init
  (load "config/theme")
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  :config
  (load-theme theme t)
  ;; theme integrations
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  (use-package doom-modeline
    :ensure t
    :hook (after-init . doom-modeline-mode)))


;; column 80 ruler
(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-width 1
	fci-rule-color "gray"
	fci-rule-column 80)
  :hook (prog-mode . fci-mode))
