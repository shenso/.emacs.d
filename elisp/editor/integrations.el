;; magit
(use-package magit
  :requires general
  :after (evil general)
  :config
  (when (featurep 'evil)
    (setup-magit-evil-bindings))
  :general
  (:states '(normal)
   :prefix "SPC" "gg" 'magit-status))

;; sudo-edit
(use-package sudo-edit
  :unless (string= system-type "windows-nt"))
