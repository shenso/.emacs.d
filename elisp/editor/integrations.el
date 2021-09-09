;; magit
(use-package magit
  :ensure t
  :after evil
  :config
  (when (featurep 'evil)
    (setup-magit-evil-bindings))
  :general
  (:states '(normal)
   :prefix "SPC" "gg" 'magit-status))

;; sudo-edit
(use-package sudo-edit
  :unless (string= system-type "windows-nt")
  :ensure t)
