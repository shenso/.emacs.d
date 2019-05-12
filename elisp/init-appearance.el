(provide 'init-appearance)

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
