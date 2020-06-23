(require 'general)
(require 'evil)

(defvar normal-state-magit-modes
  '(magit-mode
    magit-cherry-mode
    magit-diff-mode
    magit-log-mode
    magit-log-select-mode
    magit-process-mode
    magit-reflog-mode
    magit-refs-mode
    magit-revision-mode
    magit-stash-mode
    magit-stashes-mode
    magit-status-mode))

;; evil bindings for neotree
(defun setup-neotree-evil-bindings ()
  (evil-set-initial-state 'neotree-mode 'normal)
  (general-define-key
   :states 'normal
   :keymaps 'neotree-mode-map
   "j"      'neotree-next-line
   "k"      'neotree-previous-line
   "SPC"    'neotree-quick-look
   "TAB"    'neotree-quick-look
   "RET"    'neotree-quick-look
   "0 .. 9" 'digit-argument
   "g"      'beginning-of-buffer
   "G"      'end-of-buffer
   "?"      'describe-mode
   "A"      'neotree-stretch-toggle
   "C-j"    'neotree-select-down-node
   "H"      'neotree-hidden-files-toggle
   "U"      'neotree-select-up-node
   "C-g"    'neotree-refresh))

;; evil bindings for magit
(defun setup-magit-evil-bindings ()
  (dolist (mode normal-state-magit-modes)
    (evil-set-initial-state mode 'normal))
  (general-define-key
   :states 'normal
   :keymaps 'magit-mode-map
   "]" 'magit-section-forward
   "[" 'magit-section-backward
   "gr"  'magit-refresh
   "gR"  'magit-refresh-all
   "x"   'magit-delete-thing
   "X"   'magit-file-untrack
   "-"   'magit-revert-no-commit
   "_"   'magit-revert
   "p"   'magit-push
   "o"   'magit-reset-quickly
   "O"   'magit-reset
   "|"   'magit-git-command
   "'"   'magit-submodule
   "\""  'magit-subtree
   "="   'magit-diff-less-context
   "j"   'evil-next-line
   "k"   'evil-previous-line
   "gg"  'evil-goto-first-line
   "G"   'evil-goto-line
   "C-d" 'evil-scroll-down
   "C-f" 'evil-scroll-page-down
   "C-b" 'evil-scroll-page-up
   "q"   'magit-mode-bury-buffer
   "TAB" 'magit-section-toggle
   "?"   'describe-mode
   "RET" 'magit-visit-thing))
