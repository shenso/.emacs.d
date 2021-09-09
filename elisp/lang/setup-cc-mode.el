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
  :ensure t
  :init
  (setq w32-pipe-read-delay 0)
  :config
  (use-package company-irony
    :ensure t
    :after company
    :config
    (add-to-list 'company-backends 'company-irony)
    :hook (((c-mode c++-mode) . setup-cc-company)
	   ((c-mode c++-mode) . company-mode)))
  (use-package flycheck-irony
    :ensure t
    :after flycheck
    :hook (((c-mode c++-mode) . flycheck-mode)
	   (flycheck-mode . flycheck-irony-setup)))
  :hook ((c-mode . irony-mode)
	 (irony-mode . irony-cdb-autosetup-compile-options)))

