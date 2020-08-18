;; disable toolbar/gui stuff
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; line numbers
(use-package nlinum
  :ensure t
  :config
  (setq nlinum-format " %d"))
(use-package nlinum-relative
  :ensure t
  :config
  (setq nlinum-relative-redisplay-delay 0)
  :hook ((prog-mode text-mode) . nlinum-relative-mode))


;;; set theme
;; on first install do M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-moonlight t)

  (doom-themes-neotree-config)
  ;; does some fontify stuff
  (doom-themes-org-config))
;; mode line
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; font
(if (string= system-type "windows-nt")
    ;; must remove the dash on windows for some reason.
    (progn
      (set-face-attribute 'default nil :font "gohufont-11")
      (set-face-attribute 'default nil :height 80))
  (set-frame-font "-*-gohufont-*-*-*-*-11-*-*-*-*-*-*-*"))

;; column 80 ruler
(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-width 1
	fci-rule-color "gray"
	fci-rule-column 80)
  (add-hook 'prog-mode-hook 'fci-mode))
