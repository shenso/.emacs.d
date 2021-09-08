(load (concat scripts-directory "config/workspaces"))

(when (eq
       (setq workspace (cdr (assoc system-name workspaces)))
       nil)
  (setq workspace 'default))

(provide 'workspaces)
