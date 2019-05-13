;; disable toolbar/gui stuff
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; line numbers
(use-package nlinum)
(use-package nlinum-relative
  :config
  (setq nlinum-relative-redisplay-delay 0)
  :hook ((prog-mode text-mode) . nlinum-relative-mode))

;; set theme
(use-package night-owl-theme
  :config
  (load-theme 'night-owl t))

;; font
(add-to-list 'default-frame-alist '(font . "gohufont-11"))

;; column 80 ruler
(use-package fill-column-indicator
  :config
  (setq fci-rule-width 1
	fci-rule-color "gray"
	fci-rule-column 80)
  (add-hook 'prog-mode-hook 'fci-mode))
;; post-column 80 highlighting
(require 'whitespace)
(setq whitespace-line-column 80
      whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
