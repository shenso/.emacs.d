;; general emacs settings
(provide 'init-editor)

;; move backup files to ~/.emacs.d/.backups
(setq backup-directory-alist `(("." . "~/.emacs.d/.backups"))
      backup-by-copying t
      ;; handle old backups
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)