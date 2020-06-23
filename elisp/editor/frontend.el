;; features and their respective packages
(setq editor-features
      #s(hash-table
	 size 30
	 test equal
	 data ("csharp" (csharp-mode omnisharp))))

;; interactive function to install a feature
;; packages should be manually uninstalled using package-delete.
(defun editor-install-feature (feature)
  (interactive
   (list
    (completing-read "Feature: " (hash-table-keys editor-features))))
  (let ((packages (gethash feature editor-features)))
    (unless (eq packages nil)
      (package-refresh-contents)
      (dolist (package packages)
	(package-install package)))))
