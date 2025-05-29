;;; org-pretty-theme.el by Shawn Henson

;; To the extent possible under law, the person who associated CC0 with
;; org-pretty-theme.el has waived all copyright and related or neighboring rights
;; to org-pretty-theme.el.

;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

(require 'org-indent)
(require 'macros)

(defun org-pretty-font-family ()
  (coalesce-font-family
   "Gill Sans"
   "DejaVu Sans"))

(deftheme org-pretty
  "My custom org-mode theme")

(defun refresh-org-pretty-faces ()
  (let ((fixed-pitch-font (face-attribute 'fixed-pitch :font))
        (fixed-pitch-height (face-attribute 'fixed-pitch :height))
        (fixed-pitch-background (face-attribute 'fixed-pitch :background)))
    (when (org-pretty-font-family)
      (custom-theme-set-faces
       'org-pretty
       `(variable-pitch     ((((type graphic)) (:family ,(org-pretty-font-family)
                                                        :height 1.18))))
       `(org-document-title ((((type graphic)) (:font   ,(org-pretty-font-family)
                                                        :height 1.8
                                                        :weight bold))))
       `(org-level-1        ((((type graphic)) (:font   ,(org-pretty-font-family)
                                                        :height 1.3
                                                        :weight bold))))
       `(org-level-2        ((((type graphic)) (:font   ,(org-pretty-font-family)
                                                        :height 1.2
                                                        :weight bold))))
       `(org-level-3        ((((type graphic)) (:font   ,(org-pretty-font-family)
                                                        :height 1.1
                                                        :weight bold))))
       `(org-level-4        ((((type graphic)) (:font   ,(org-pretty-font-family)
                                                        :height 1.1
                                                        :weight bold))))
       `(org-level-5        ((((type graphic)) (:font   ,(org-pretty-font-family)
                                                        :height 1.1
                                                        :weight bold))))
       `(org-level-6        ((((type graphic)) (:font   ,(org-pretty-font-family)
                                                        :height 1.1
                                                        :weight bold))))
       `(org-level-7        ((((type graphic)) (:font   ,(org-pretty-font-family)
                                                        :height 1.1
                                                        :weight bold))))
       `(org-level-8        ((((type graphic)) (:font   ,(org-pretty-font-family)
                                                        :height 1.1
                                                        :weight bold))))

       '(org-block            ((((type graphic))
                                (:inherit fixed-pitch
                                          :height 0.85
                                          :foreground unspecified))))
       '(org-code             ((((type graphic))
                                (:inherit (shadow fixed-pitch)
                                          :height 0.85))))
       '(org-indent           ((((type graphic))
                                (:inherit org-hide))))
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
                                (:inherit fixed-pitch))))
       `(org-footnote
         ((((type graphic)) (:font ,fixed-pitch-font
                                   :height ,fixed-pitch-height))))
       `(org-hide
         ((((type graphic))
           (:foreground ,fixed-pitch-background
                        :background ,fixed-pitch-background)))))))

  (custom-theme-set-variables
   'org-pretty
   '(org-startup-indented t)
   '(org-pretty-entities t)
   '(org-adapt-indentation t)
   '(org-hide-leading-stars t)
   '(org-hide-emphasis-markers t)))

(defun reload-org-pretty ()
  (interactive)
  (refresh-org-pretty-faces)
  (when (memq 'org-pretty custom-enabled-themes)
    (disable-theme 'org-pretty)
    (enable-theme 'org-pretty)))

(call-on-client-frame-init reload-org-pretty)

(provide-theme 'org-pretty)
(provide 'org-pretty-theme)
