;;; shenso-secrets.el --- secret loader -*- lexical-binding: t; -*-

;; To the extent possible under law, the person who associated CC0 with
;; init.el has waived all copyright and related or neighboring rights
;; to init.el.

;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

;;; Commentary:

;;; Code:

(require 'epa)

(defvar shenso-secrets-file-loaded nil)
(defvar shenso-secrets-available nil)

(defun shenso-secrets-claude-api-key ()
  (error "Claude API key is not available!"))

(defun shenso-load-secrets ()
  (unless (memq epa-file-handler file-name-handler-alist)
    (epa-file-enable))

  (condition-case err
      (progn
        (load-file (expand-file-name "secrets.el.gpg" user-emacs-directory))
        (setq shenso-secrets-file-loaded t))
    (error
     (display-warning 'shenso-secrets
                      (format "Could not load secrets: %s"
                              (error-message-string err))
                      :warning
                      "*Warnings*"))))

(defun shenso-ensure-secrets ()
  "Load secrets if they haven't been already."
  (unless shenso-secrets-file-loaded
    (shenso-load-secrets)))

(provide 'shenso-secrets)
;;; shenso-secrets.el ends here
