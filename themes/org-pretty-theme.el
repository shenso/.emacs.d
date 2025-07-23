;;; org-pretty-theme.el by Shawn Henson -*- lexical-binding: t -*-

;; To the extent possible under law, the person who associated CC0 with
;; org-pretty-theme.el has waived all copyright and related or neighboring rights
;; to org-pretty-theme.el.

;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

(require 'org-indent)
(require 'shenso-macros)

(deftheme org-pretty
  "My custom org-mode theme")

(defvar org-pretty-font-family nil)

(defun org-pretty-find-font-family ()
  (any-font-family
   "Gill Sans"
   "DejaVu Sans"))

(custom-theme-set-variables
   'org-pretty
   '(org-startup-indented t)
   '(org-pretty-entities t)
   '(org-adapt-indentation t)
   '(org-hide-leading-stars t)
   '(org-hide-emphasis-markers t))

(custom-theme-set-faces
 'org-pretty
 '(org-block            ((((type graphic))
                          (:inherit fixed-pitch
                                    :height 0.85
                                    :foreground unspecified))))
 '(org-code             ((((type graphic))
                          (:inherit (shadow fixed-pitch)
                                    :height 0.85))))
 '(org-verbatim         ((((type graphic))
                          (:inherit (shadow fixed-pitch)
                                    :height 0.85))))
 '(org-special-keyword  ((((type graphic))
                          (:inherit (font-lock-comment-face
                                     fixed-pitch)))))
 '(org-meta-line        ((((type graphic))
                          (:inherit (font-lock-comment-face
                                     fixed-pitch)))))
 '(org-checkbox         ((((type graphic))
                          (:inherit fixed-pitch))))
 '(org-table            ((((type graphic))
                          (:inherit fixed-pitch)))))

(defun org-pretty-set-org-hide-face (&optional now)
  (let ((fixed-pitch-background (face-attribute 'fixed-pitch :background)))
    (custom-theme-set-faces
     'org-pretty
     `(org-hide ((((type graphic)) (:foreground
                                    ,fixed-pitch-background
                                    :background
                                    ,fixed-pitch-background)))
                ,now)
     `(org-indent ((((type graphic)) (:inherit org-hide))) ,now))))

(defun org-pretty-set-face-fonts (&optional force)
  (unless (and org-pretty-font-family (not force))
    (setq org-pretty-font-family (org-pretty-find-font-family))
    (let ((fixed-pitch-font (face-attribute 'fixed-pitch :font))
          (fixed-pitch-height (face-attribute 'fixed-pitch :height)))
      (custom-theme-set-faces
       'org-pretty
       `(variable-pitch     ((((type graphic)) (:family ,org-pretty-font-family
                                                        :height 1.18))))
       `(org-document-title ((((type graphic)) (:font   ,org-pretty-font-family
                                                        :height 1.8
                                                        :weight bold))))
       `(org-level-1        ((((type graphic)) (:font   ,org-pretty-font-family
                                                        :height 1.3
                                                        :weight bold))))
       `(org-level-2        ((((type graphic)) (:font   ,org-pretty-font-family
                                                        :height 1.2
                                                        :weight bold))))
       `(org-level-3        ((((type graphic)) (:font   ,org-pretty-font-family
                                                        :height 1.1
                                                        :weight bold))))
       `(org-level-4        ((((type graphic)) (:font   ,org-pretty-font-family
                                                        :height 1.1
                                                        :weight bold))))
       `(org-level-5        ((((type graphic)) (:font   ,org-pretty-font-family
                                                        :height 1.1
                                                        :weight bold))))
       `(org-level-6        ((((type graphic)) (:font   ,org-pretty-font-family
                                                        :height 1.1
                                                        :weight bold))))
       `(org-level-7        ((((type graphic)) (:font   ,org-pretty-font-family
                                                        :height 1.1
                                                        :weight bold))))
       `(org-level-8        ((((type graphic)) (:font   ,org-pretty-font-family
                                                        :height 1.1
                                                        :weight bold))))
       `(org-footnote
         ((((type graphic)) (:font ,fixed-pitch-font
                                   :height ,fixed-pitch-height))))))))

(defun org-pretty-reload ()
  (interactive)
  (org-pretty-set-org-hide-face t)
  (when (memq 'org-pretty custom-enabled-themes)
    (disable-theme 'org-pretty)
    (enable-theme 'org-pretty)))

(defun org-pretty-full-reload ()
  (interactive)
  (org-pretty-set-org-hide-face t)
  (org-pretty-set-face-fonts t) ; fuck this fucking bullshit
  (when (memq 'org-pretty custom-enabled-themes)
    (disable-theme 'org-pretty)
    (enable-theme 'org-pretty)))

(when (or (not (daemonp)) after-init-time)
  (org-pretty-full-reload))

(add-hook 'server-after-make-frame-hook #'org-pretty-full-reload)

(provide-theme 'org-pretty)
;;; org-pretty-theme.el ends here
