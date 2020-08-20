;; configurations for editing files in c, c++, java, etc.
;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html

(setq-default c-basic-offset 4
	      c-default-style '((java-mode . "java")
				(other . "k&r")))

;; editor settings to apply only in c-mode/c++-mode
(defun setup-cc-indentation ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4))
(add-hook 'c-mode-hook 'setup-cc-indentation)
(add-hook 'c++-mode-hook 'setup-cc-indentation)

(defun setup-cc-company ()
  (setq company-idle-delay 0))

;; autocompletion
(use-package irony
  :init
  (setq w32-pipe-read-delay 0)
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :requires (company irony)
  :config
  (add-to-list 'company-backends 'company-irony)
  (add-hook 'c++-mode-hook 'setup-cc-company)
  (add-hook 'c-mode-hook 'setup-cc-company)
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode))

(use-package flycheck-irony
  :requires (flycheck irony)
  :config
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))
