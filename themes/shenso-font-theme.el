;;; shenso-font-theme.el by Shawn Henson

;; To the extent possible under law, the person who associated CC0 with
;; shenso-font-theme.el has waived all copyright and related or neighboring rights
;; to shenso-font-theme.el.

;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

(require 'shenso-macros)

(defgroup shenso-font nil
  "Font customization."
  :prefix "shenso-font-"
  :group 'faces)

(defcustom shenso-font-use-small-fonts nil
  "Uses smaller sized fonts when true."
  :type '(boolean)
  :group 'shenso-font)

;; https://media.tenor.com/9fXqFyfBPVwAAAAM/too-tiny-ken-jeong.gif
(defun shenso-font--small-font ()
  (any-font
   "-*-Menlo-regular-normal-normal-*-11-*-*-*-m-0-iso10646-1"
   (concat "DejaVu Sans Mono:pixelsize=13:foundry=PfEd:weight=regular"
           ":slant=normal:width=normal:spacing=100:scalable=true")
   (face-attribute 'default :font)))

(defun shenso-font--large-font ()
  (any-font
   "-*-Menlo-regular-normal-normal-*-13-*-*-*-m-0-iso10646-1"
   (concat "DejaVu Sans Mono:pixelsize=15:foundry=PfEd:weight=regular"
           ":slant=normal:width=normal:spacing=100:scalable=true")))

(defun shenso-font-name ()
  (if shenso-font-use-small-fonts
      (shenso-font--small-font)
    (shenso-font--large-font)))

(deftheme shenso-font
  "My basic font theme")

(defun refresh-shenso-font-faces ()
  (interactive)
  (let ((font-name (shenso-font-name)))
    (when font-name
      (custom-theme-set-faces
       'shenso-font
       `(default     ((((type graphic)) (:font ,font-name))))
       `(fixed-pitch ((((type graphic)) (:font ,font-name))))))))

(defun reload-shenso-font ()
  (interactive)
  (refresh-shenso-font-faces)
  (when (memq 'shenso-font custom-enabled-themes)
    (disable-theme 'shenso-font))
  (enable-theme 'shenso-font))

(provide-theme 'shenso-font)
(provide 'shenso-font-theme)
