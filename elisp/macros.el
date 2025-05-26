;;; macros.el by Shawn Henson

;; To the extent possible under law, the person who associated CC0 with
;; macros.el has waived all copyright and related or neighboring rights
;; to macros.el.

;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

(defmacro coalesce (&rest vals)
  (append '(cl-find-if #'identity) `(,(append '(list) vals))))

(defmacro set-user-dir (name val &rest kvs)
  (when (/= (% (length kvs) 2) 0)
    (error (format "Wrong number of arguments: %d" (+ (length kvs) 2))))
  (defun dir-arg-list (raw-list)
    (if (>= (length raw-list) 2)
        (let ((s-key (car raw-list))
              (s-val (car (cdr raw-list))))
          (append `(,s-key (when ,s-val (file-name-as-directory (expand-file-name ,s-val))))
                  (dir-arg-list (cddr raw-list))))
      nil))
  (append '(setq)
          `(,name (when ,val (file-name-as-directory (expand-file-name ,val))))
          (dir-arg-list kvs)))

(defmacro coalesce-font-family (&rest vals)
  (defun family-available-p (family-name)
    (member family-name (font-family-list)))
  (append '(coalesce)
          (mapcar (lambda (name)
                    `(car (family-available-p ,name)))
                  vals)))

(defmacro coalesce-font (&rest vals)
  (append '(coalesce)
          (mapcar (lambda (name)
                    `(when (find-font (font-spec :name ,name)) ,name))
                  vals)))

(defmacro call-on-client-frame-init (fn-name)
  `(if (daemonp)
       (add-hook 'server-after-make-frame-hook #',fn-name)
     (,fn-name)))

(provide 'macros)
