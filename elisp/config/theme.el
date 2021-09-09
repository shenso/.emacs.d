(require 'workspaces)

;; theme
(setq-default theme 'doom-moonlight)
;; font
(cond ((eq workspace 'work)
       (set-face-attribute 'default nil :font "Anonymous Pro" :height 115))
      ((eq workspace 'home)
       (set-face-attribute 'default nil :font "Gohu GohuFont")))

