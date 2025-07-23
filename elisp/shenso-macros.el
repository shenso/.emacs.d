;;; shenso-macros.el by Shawn Henson -*- lexical-binding: t -*-

;; To the extent possible under law, the person who associated CC0 with
;; macros.el has waived all copyright and related or neighboring rights
;; to shenso-macros.el.

;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

(defmacro set-user-dir (name val &rest kvs)
  (when (/= (% (length kvs) 2) 0)
    (error (format "Wrong number of arguments: %d" (+ (length kvs) 2))))
  (defun dir-arg-list (raw-list)
    (if (>= (length raw-list) 2)
        (let ((s-key (car raw-list))
              (s-val (car (cdr raw-list))))
          (append `(,s-key (when ,s-val
                             (file-name-as-directory (expand-file-name ,s-val))))
                  (dir-arg-list (cddr raw-list))))
      nil))
  (append '(setq)
          `(,name (when ,val (file-name-as-directory (expand-file-name ,val))))
          (dir-arg-list kvs)))

(defmacro abs-dir (rel-dir)
  "Return the absolute path of REL-DIR as a directory, if non-nil."
  `(let ((rel-dir ,rel-dir))
     (if rel-dir
         (expand-file-name (file-name-as-directory ,rel-dir)))))

(defmacro any-abs-dir (&rest rel-dirs)
  "Return the absolute path of the first directory in REL-DIRS which exists."
  `(or ,@(mapcar (lambda (rel-dir)
                   `(let ((abs-path (abs-dir ,rel-dir)))
                      (if (and abs-path (file-exists-p abs-path))
                          abs-path)))
                 rel-dirs)))

(defmacro any-font-family (&rest vals)
  `(or ,@(mapcar (lambda (name)
                   `(car (member ,name (font-family-list))))
                 vals)))

(defmacro any-font (&rest vals)
  `(or ,@(mapcar
          (lambda (name)
            `(if (find-font (font-spec :name ,name)) ,name))
          vals)))

(defmacro call-on-client-frame-init (fun)
  `(if (daemonp)
       (add-hook 'server-after-make-frame-hook ,fun)
     (funcall ,fun)))

(provide 'shenso-macros)
;;; shenso-macros.el ends here
