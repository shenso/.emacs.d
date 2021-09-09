(load (concat scripts-directory "config/workspaces"))

(defvar workspaces ()
  "An association list of system-names and their respective workspaces.")
(defvar workspace 'default
  "The workspace currently in use. To be determined by the
  association list.")

(when (eq
       (setq workspace (cdr (assoc system-name workspaces)))
       nil)
  (setq workspace 'default))

(provide 'workspaces)
