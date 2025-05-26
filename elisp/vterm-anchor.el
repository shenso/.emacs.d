;;; vterm-anchor.el by Shawn Henson

;; To the extent possible under law, the person who associated CC0 with
;; vterm-anchor.el has waived all copyright and related or neighboring rights
;; to vterm-anchor.el.

;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

;; a simple package which starts vterm anchored to the bottom of the screen

(require 'vterm)

(defvar vterm-anchor-display-buffer-action
  '("\\*vterm\\*"
    (display-buffer-at-bottom)
    (window-height . 12))
  "The action to perform when displaying vterm buffors when the anchor is enabled.")

(setq vterm--anchored-to-bottom nil)

(defun anchor-vterm-to-bottom ()
  (interactive)
  (add-to-list 'display-buffer-alist vterm-anchor-display-buffer-action)
  (setq vterm--anchored-to-bottom t))

(defun unanchor-vterm-from-bottom ()
  (interactive)
  (setq display-buffer-alist (delete vterm-anchor-display-buffer-action display-buffer-alist))
  (setq vterm--anchored-to-bottom nil))

(defun toggle-vterm-anchor ()
  (interactive)
  (if vterm--anchored-to-bottom
      (unanchor-vterm-from-bottom)
    (anchor-vterm-to-bottom)))

(defun vterm-anchored-to-bottom-p ()
    vterm--anchored-to-bottom)

(defun vterm-anchor-handle-buffer-exit ()
    (defun vterm-anchor-kill-buffer-callback ()
      (when (and  vterm-at-bottom-enabled
                 (> (length (window-list)) 1))
        (delete-window (get-buffer-window (current-buffer)))))
    (add-hook 'kill-buffer-hook #'vterm-anchor-kill-buffer-callback nil t))

(provide 'vterm-anchor)
