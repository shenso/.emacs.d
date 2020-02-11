;;
;; functionality
;;
(require 'org)
(setq notes-path "~/documents/notes/")

;; org-agenda
(setq org-agenda-files (list notes-path
			     (concat notes-path "courses/2020-spring")))
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

;; fix code block indentation (sorta, "C-c '" is still better)
(setq org-src-tab-acts-natively t)


;;
;; form
;;
(setq org-hide-emphasis-markers t
      org-pretty-entities t
      org-startup-indented t)
;; dot character for list
(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Better header bullet points
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Headline fonts & sizes
(let* ((variable-tuple
	(cond ((x-list-fonts "Roboto") '(:font "Roboto"))
	      ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
	      ((x-family-fonts "Sans Serif") '(:font "Sans Serif"))
	      (nil (warn "Cannot find org-mode font."))))
       (base-font-color (face-foreground 'default nil 'default))
       (headline       `(:inherit default :weight bold ,base-font-color)))
  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

;; text & monospace fonts
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Roboto" :height 110 :weight light))))
 '(fixed-pitch ((t (:family "gohufont" :slant normal :weight normal :height 1.0 :width normal))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch))))))
(add-hook 'org-mode-hook 'variable-pitch-mode)
