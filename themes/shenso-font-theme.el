;;; shenso-font-theme.el by Shawn Henson

;; To the extent possible under law, the person who associated CC0 with
;; shenso-font-theme.el has waived all copyright and related or neighboring rights
;; to shenso-font-theme.el.

;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

(require 'macros)

(defun shenso-font-name ()
  (coalesce-font
   "-*-Menlo-regular-normal-normal-*-11-*-*-*-m-0-iso10646-1"
   "DejaVu Sans Mono:pixelsize=13:foundry=PfEd:weight=regular:slant=normal:width=normal:spacing=100:scalable=true"))

(deftheme shenso-font
  "My basic font theme")

(custom-theme-set-faces
 'shenso-font
 `(default     ((nil (:font ,(shenso-font-name)))))
 `(fixed-pitch ((nil (:font ,(shenso-font-name))))))

(provide-theme 'shenso-font)
(provide 'shenso-font-theme)
