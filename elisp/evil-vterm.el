;;; evil-term.el by Shawn Henson

;; To the extent possible under law, the person who associated CC0 with
;; evil-term.el has waived all copyright and related or neighboring rights
;; to evil-term.el.

;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

;; disables evil-mode on initialization, and disables it whenever switching to
;; a vterm buffer window

(defun evil-vterm-handle-window-change-selection (&optional window)
  (when (eq major-mode 'vterm-mode)
    (call-interactively 'turn-off-evil-mode)))

(defun evil-vterm-init ()
  (add-hook 'vterm-mode-hook #'turn-off-evil-mode)
  (add-hook 'buffer-list-update-hook #'evil-vterm-handle-window-change-selection)
  (add-hook 'window-selection-change-functions #'evil-vterm-handle-window-change-selection))

(provide 'evil-vterm)
