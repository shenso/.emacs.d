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

(custom-theme-set-faces
 'org-pretty
 `(variable-pitch     ((nil (:family ,(org-pretty-font-family) :height 1.18             ))))
 `(org-document-title ((nil (:font   ,(org-pretty-font-family) :height 1.8  :weight bold))))
 `(org-level-1        ((nil (:font   ,(org-pretty-font-family) :height 1.3  :weight bold))))
 `(org-level-2        ((nil (:font   ,(org-pretty-font-family) :height 1.2  :weight bold))))
 `(org-level-3        ((nil (:font   ,(org-pretty-font-family) :height 1.1  :weight bold))))
 `(org-level-4        ((nil (:font   ,(org-pretty-font-family) :height 1.1  :weight bold))))
 `(org-level-5        ((nil (:font   ,(org-pretty-font-family) :height 1.1  :weight bold))))
 `(org-level-6        ((nil (:font   ,(org-pretty-font-family) :height 1.1  :weight bold))))
 `(org-level-7        ((nil (:font   ,(org-pretty-font-family) :height 1.1  :weight bold))))
 `(org-level-8        ((nil (:font   ,(org-pretty-font-family) :height 1.1  :weight bold))))

 '(org-block            ((nil (:inherit fixed-pitch             :height 0.85 :foreground unspecified))))
 '(org-code             ((nil (:inherit (shadow fixed-pitch)    :height 0.85                        ))))
 '(org-indent           ((nil (:inherit org-hide                                                    ))))
 '(org-verbatim         ((nil (:inherit (shadow fixed-pitch)    :height 0.85                        ))))
 '(org-special-keyword  ((nil (:inherit (font-lock-comment-face
                                         fixed-pitch)))))
 '(org-meta-line        ((nil (:inherit (font-lock-comment-face
                                         fixed-pitch)))))
 '(org-checkbox         ((nil (:inherit fixed-pitch)))))

(custom-theme-set-variables
 'org-pretty
 '(org-startup-indented t)
 '(org-pretty-entities t)
 '(org-adapt-indentation t)
 '(org-hide-leading-stars t)
 '(org-hide-emphasis-markers t))

(provide-theme 'org-pretty)
(provide 'org-pretty-theme)
