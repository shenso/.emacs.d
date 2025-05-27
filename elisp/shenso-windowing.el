;;; shenso-windowing.el by Shawn Henson

;; To the extent possible under law, the person who associated CC0 with
;; shenso-windowing.el has waived all copyright and related or neighboring rights
;; to shenso-windowing.el.

;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

;; Commentary:

;; window management utilities

(defun split-window-insensibly (&optional window)
  "A near copy of `split-window-sensibly', but prefers splitting to the right
over splitting to the bottom."
    (let ((window (or window (selected-window))))
      (or (and (window-splittable-p window t)
	           ;; Split window horizontally.
	           (with-selected-window window
	             (split-window-right)))
	      (and (window-splittable-p window)
	           ;; Split window vertically.
	           (with-selected-window window
	             (split-window-below)))
	      (and
           ;; If WINDOW is the only usable window on its frame (it is
           ;; the only one or, not being the only one, all the other
           ;; ones are dedicated) and is not the minibuffer window, try
           ;; to split it vertically disregarding the value of
           ;; `split-height-threshold'.
           (let ((frame (window-frame window)))
             (or
              (eq window (frame-root-window frame))
              (catch 'done
                (walk-window-tree (lambda (w)
                                    (unless (or (eq w window)
                                                (window-dedicated-p w))
                                      (throw 'done nil)))
                                  frame nil 'nomini)
                t)))
	       (not (window-minibuffer-p window))
	       (let ((split-height-threshold 0))
	         (when (window-splittable-p window)
	           (with-selected-window window
	             (split-window-below))))))))

(provide 'shenso-windowing)
