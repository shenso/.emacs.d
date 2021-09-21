(use-package web-mode
  :ensure t
  :after php-mode
  :init
  (setq web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 4
	web-mode-code-indent-offset 4)
  (setq web-mode-ac-sources-alist
	'(("php" . (company-ac-php-backend))))
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  ;;(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  ;; no php until company issues resolved
  :hook
  (web-mode . (lambda ()
		(setq indent-tabs-mode nil
		      tab-width 4))))

(defmacro add-web-mode-hook (extension body)
  `(add-hook 'web-mode-before-autocomplete-hook
	     (lambda ()
	       (when (string= (file-name-extension buffer-file-name)
			      ,extension)
		 ,body))))

(provide 'setup-web-mode)
