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
   "ETBembo"
   "Gill Sans"
   "DejaVu Sans"))

(defconst org-pretty-bg-main "#f5f5f5")
(defconst org-pretty-fg-main "#2e3338")
(defconst org-pretty-bg-active "#f2f3f5")
(defconst org-pretty-bg-inactive "#e3e5e8")
(defconst org-pretty-header-line-fg "#2e3338")

(defconst org-pretty-level-1-fg "#3f578f")
(defconst org-pretty-level-2-fg "#005f5f")
(defconst org-pretty-level-3-fg "#a0132f")
(defconst org-pretty-level-4-fg "#702000")
(defconst org-pretty-level-5-fg "#3548cf")
(defconst org-pretty-level-6-fg "#00663f")
(defconst org-pretty-level-7-fg "#721045")
(defconst org-pretty-level-8-fg "#005e8b")

(defconst org-pretty-info-keyword-fg "#6b7386")
(defconst org-pretty-table-fg "#526778")

(defconst org-pretty-margin-width 8)

(defgroup org-pretty nil
  "org-pretty theme customizations."
  :group 'org-appearance)

(defcustom org-pretty-bullet-list
  '("◉" "○" "✸" "✿" "◆" "◇" "★" "▸")
  "List of bullets used in Org headings.
It can contain any number of symbols, which will be repeated."
  :group 'org-pretty
  :type '(repeat (string :tag "Bullet character")))

(defcustom org-pretty-bulling-padding
  '(0.5 0.5 0.5 0.3 0.2 0.2 0.2 0.2)
  "List of bullets used in Org headings.
It can contain any number of symbols, which will be repeated."
  :group 'org-pretty
  :type '(repeat (string :tag "Bullet character")))

(custom-theme-set-variables
   'org-pretty
   '(org-startup-indented nil)
   '(org-pretty-entities t)
   '(org-adapt-indentation nil)
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
 `(org-table            ((((type graphic))
                          (:inherit fixed-pitch
                           :foreground ,org-pretty-table-fg)))))

(defun org-pretty-set-face-fonts (&optional force)
  (unless (and org-pretty-font-family (not force))
    (setq org-pretty-font-family (org-pretty-find-font-family))
    (let ((fixed-pitch-font (face-attribute 'fixed-pitch :font))
          (fixed-pitch-height (face-attribute 'fixed-pitch :height)))
      (custom-theme-set-faces
       'org-pretty
       `(variable-pitch     ((((type graphic)) (:family ,org-pretty-font-family
                                                        :height 1.25))))
       `(org-hide           ((((type graphic)) (:foreground ,org-pretty-bg-main
                                                :background ,org-pretty-bg-main))))
       `(org-indent         ((((type graphic)) (:inherit org-hide))))
       `(org-document-title ((((type graphic)) (:font   ,org-pretty-font-family
                                                :height 1.8
                                                :weight bold
                                                :foreground ,org-pretty-fg-main))))
       `(org-document-info-keyword
         ((((type graphic)) (:inherit default
                             :height 0.8
                             :weight light
                             :foreground ,org-pretty-info-keyword-fg))))
       `(org-document-info  ((((type graphic)) (:inherit variable-pitch
                                                :foreground ,org-pretty-fg-main))))
       `(org-level-1        ((((type graphic)) (:inherit default
                                                :font   ,org-pretty-font-family
                                                :foreground ,org-pretty-level-1-fg
                                                :height 1.7
                                                :weight bold))))
       `(org-level-2        ((((type graphic)) (:inherit default
                                                :font   ,org-pretty-font-family
                                                :foreground ,org-pretty-level-2-fg
                                                :height 1.5
                                                :weight bold))))
       `(org-level-3        ((((type graphic)) (:inherit default
                                                :font   ,org-pretty-font-family
                                                :foreground ,org-pretty-level-3-fg
                                                :height 1.3
                                                :weight bold))))
       `(org-level-4        ((((type graphic)) (:inherit default
                                                :font   ,org-pretty-font-family
                                                :foreground ,org-pretty-level-4-fg
                                                :height 1.1
                                                :weight bold))))
       `(org-level-5        ((((type graphic)) (:inherit default
                                                :font   ,org-pretty-font-family
                                                :foreground ,org-pretty-level-5-fg
                                                :height 1.1
                                                :weight bold))))
       `(org-level-6        ((((type graphic)) (:inherit default
                                                :font   ,org-pretty-font-family
                                                :foreground ,org-pretty-level-6-fg
                                                :height 1.1
                                                :weight bold))))
       `(org-level-7        ((((type graphic)) (:inherit default
                                                :font   ,org-pretty-font-family
                                                :foreground ,org-pretty-level-7-fg
                                                :height 1.1
                                                :weight bold))))
       `(org-level-8        ((((type graphic)) (:inherit default
                                                :font   ,org-pretty-font-family
                                                :foreground ,org-pretty-level-8-fg
                                                :height 1.1
                                                :weight bold))))
       `(org-footnote
         ((((type graphic)) (:font ,fixed-pitch-font
                             :height ,fixed-pitch-height))))))))

(defun org-pretty-reload ()
  (interactive)
  (when (memq 'org-pretty custom-enabled-themes)
    (disable-theme 'org-pretty)
    (enable-theme 'org-pretty)))

(defun org-pretty-full-reload ()
  (interactive)
  (org-pretty-set-face-fonts t) ; fuck this fucking bullshit
  (when (memq 'org-pretty custom-enabled-themes)
    (disable-theme 'org-pretty)
    (enable-theme 'org-pretty)))

(defun org-pretty-header-line-text (margin-width)
  (let* ((buffer-name (buffer-name))
         (title (org-get-title))
         (width (- (window-width) 1)))
    (truncate-string-to-width (or title buffer-name) width nil ?  "...")))

(defun org-pretty-build-header-line-format (margin-width margin-height padding)
  (list (propertize " " 'display `((raise ,(+ margin-height padding))
                                   (space :width ,margin-width)))
        `(:eval (propertize (org-pretty-header-line-text ,margin-width)
                            'face `(:underline (:color ,(face-foreground 'default)
                                                :position 8))
                            'display '(raise ,margin-height)))
        (propertize " " 'display `((raise ,(+ margin-height padding))
                                   (space :width ,margin-width)))))

(defun org-pretty-level-char (level)
  (string-to-char
   (nth (mod (/ (1- level) (if org-odd-levels-only 2 1))
             (length org-pretty-bullet-list))
        org-pretty-bullet-list)))

(defun org-pretty-setup-buffer ()
  (when (memq 'org-pretty custom-enabled-themes)
    (setq left-margin-width org-pretty-margin-width
          right-margin-width org-pretty-margin-width)

    (face-remap-add-relative 'default
                             :background org-pretty-bg-main
                             :foreground org-pretty-fg-main)
    (face-remap-add-relative 'fixed-pitch
                             :background org-pretty-bg-main
                             :foreground org-pretty-fg-main)
    (face-remap-add-relative 'org-hide
                             :foreground org-pretty-bg-main)
    (face-remap-add-relative 'header-line
                             :background org-pretty-bg-main
                             :foreground org-pretty-header-line-fg)
    (set-window-buffer nil (current-buffer))
    (fringe-mode 0)
    (setq header-line-format (org-pretty-build-header-line-format
                              org-pretty-margin-width 1.0 1.0))


    (let ((margin-format (format "%%%ds" left-margin-width)))
      (font-lock-add-keywords
       nil
       `(("^\\*+ "
          (0 (let* ((level (- (match-end 0) (match-beginning 0) 1))
                    (level-face (nth (if org-cycle-level-faces
                                         (% (1- level) org-n-level-faces)
                                       (1- (min level org-n-level-faces)))
                                     org-level-faces)))
               (put-text-property
                (match-beginning 0)
                (match-end 0)
                'display
                `((margin left-margin)
                  ,(propertize (format
                                ,margin-format
                                (concat (char-to-string
                                         (org-pretty-level-char level))
                                        "  "))
                               'display '(raise (/ 1 (- level-height 1.2)))
                               'face '(:inherit fixed-pitch
                                       :height 1.2
                                       :weight light))
                  append))
               nil))))))

    (save-restriction
      (widen)
      (font-lock-flush)
      (font-lock-ensure))
    (with-no-warnings
      (font-lock-fontify-buffer))))



(when (or (not (daemonp)) after-init-time)
  (org-pretty-full-reload))

(add-hook 'server-after-make-frame-hook #'org-pretty-full-reload)
(add-hook 'org-mode-hook #'org-pretty-setup-buffer)

(provide-theme 'org-pretty)
;;; org-pretty-theme.el ends here
