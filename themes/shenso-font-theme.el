;;; shenso-font-theme.el by Shawn Henson

;; To the extent possible under law, the person who associated CC0 with
;; shenso-font-theme.el has waived all copyright and related or neighboring rights
;; to shenso-font-theme.el.

;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

(require 'macros)

(defcustom shenso-font-use-small-fonts nil
  "Uses smaller sized fonts when true."
  :type '(boolean))

;; https://media.tenor.com/9fXqFyfBPVwAAAAM/too-tiny-ken-jeong.gif
(defun shenso-font--small-fonts ()
  (coalesce-font
   "-*-Menlo-regular-normal-normal-*-11-*-*-*-m-0-iso10646-1"
   (concat "DejaVu Sans Mono:pixelsize=13:foundry=PfEd:weight=regular"
           ":slant=normal:width=normal:spacing=100:scalable=true")
   (face-attribute 'default :font)))

(defun shenso-font--large-fonts ()
  (coalesce-font
   "-*-Menlo-regular-normal-normal-*-13-*-*-*-m-0-iso10646-1"
   (concat "DejaVu Sans Mono:pixelsize=15:foundry=PfEd:weight=regular"
           ":slant=normal:width=normal:spacing=100:scalable=true")))

(defun shenso-font-name ()
  (if shenso-font-use-small-fonts
      (shenso-font--small-fonts)
    (shenso-font--large-fonts)))

(deftheme shenso-font
  "My basic font theme")

(defun refresh-shenso-font-faces ()
  (interactive)
  (when (shenso-font-name)
    (custom-theme-set-faces
     'shenso-font
     `(default     ((((type graphic)) (:font ,(shenso-font-name)))))
     `(fixed-pitch ((((type graphic)) (:font ,(shenso-font-name))))))))

(defun reload-shenso-font ()
  (interactive)
  (refresh-shenso-font-faces)
  (when (memq 'shenso-font custom-enabled-themes)
    (disable-theme 'shenso-font)
    (enable-theme 'shenso-font)))

(call-on-client-frame-init reload-shenso-font)

(provide-theme 'shenso-font)
(provide 'shenso-font-theme)
