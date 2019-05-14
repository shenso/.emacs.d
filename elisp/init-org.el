(require 'org)
(setq notes-path "~/documents/notes/")

;; org-agenda
(setq org-agenda-files (list notes-path
			     (concat notes-path "agenda")))
(global-set-key (kbd "C-c a") 'org-agenda)

;; org-crypt
(require 'org-crypt)
(setq org-crypt-key "shawnhenson@knights.ucf.edu")

;; org-journal
(use-package org-journal
  :config
  (setq org-journal-dir (concat notes-path "journal")
	org-journal-file-type 'weekly
	org-journal-enable-encyption t
	org-journal-encrypt-journal t))
