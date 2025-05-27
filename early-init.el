(setq package-enable-at-startup nil)

(setq gc-cons-threshold 104857600) ; 100mb
(setq read-process-output-max (* 1048576)) ; 1mb

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)
