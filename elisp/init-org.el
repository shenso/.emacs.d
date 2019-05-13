(provide 'init-org)

;; org-crypt
(require 'org-crypt)
(setq org-crypt-key "shawnhenson@knights.ucf.edu")

;; org-journal
(use-package org-journal
  :config
  (setq org-journal-dir "~/documents/notes/journal"
	org-journal-file-type 'weekly
	org-journal-enable-encyption t
	org-journal-encrypt-journal t))
