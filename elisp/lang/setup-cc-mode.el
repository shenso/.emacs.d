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
