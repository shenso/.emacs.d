;; -*- lexical-binding: t; -*-

;;; csv-rainbow.el --- make columns in csv/tsv files colorful!

;; Copyright (c) 2025 Shawn Henson

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Commentary:

;; Avoids regular expressions unlike all the other solutions I've seen,
;; and handles quoting too. Depends on csv-mode, though that may change as
;; csv-mode appears to perform rather poorly on large files.

(require 'csv-mode)
(require 'color)

;; ansi colors by default
(defcustom csv-rainbow-ansi-colors
  '("red" "green" "yellow" "blue" "magenta" "cyan")
  "Colors csv-rainbow will display on terminals.")

(defcustom csv-rainbow-graphic-colors
  '("orange red" "lawn green" "orange" "deep sky blue" "magenta" "sea green"
    "medium turquoise")
  "Colors csv-rainbow will display on GUI clients.")

(defun csv-rainbow-color-codes ()
  (mapcar
   (lambda (c)
     (apply 'format "#%02x%02x%02x" c))
   (mapcar
    (lambda (c)
      (mapcar (lambda (cs) (ash cs -8)) (color-values c)))
    (seq-filter (lambda (color) (member color (defined-colors)))
                (if (display-graphic-p)
                    csv-rainbow-graphic-colors
                  csv-rainbow-ansi-colors)))))

(defmacro csv-rainbow--define-faces ()
  (let ((faces nil)
        (face-symbols nil)
        (color-codes (csv-rainbow-color-codes)))
    (dotimes (i (length color-codes))
      (let ((face-symb (intern (format "rainbow-csv-face-%d-face" i)))
            (face-color (nth i color-codes)))
        (push
         `(defface ,face-symb
            '((default (:inherit 'unspecified))
              ;; TODO: different colors per background
              (((class color) (background light)) :foreground ,face-color)
              (((class color) (background dark)) :foreground ,face-color))
            ,(format "CSV Rainbow Face %d" i))
         faces)
        (push face-symb face-symbols)))
    `(progn
       ,@faces
       (setq csv-rainbow--faces ',face-symbols))))

(defun csv-rainbow--forward-delimiter (separators field-quotes end
                                                  &optional quote-level)
  (unless quote-level
    (setq quote-level 0))
  (let ((found nil)
        (at-newline nil))
    (while (and (not found)
                (not at-newline)
                (<= (point) end)
                (< (point) (point-max)))
      (cond ((memq (char-after) field-quotes)
             (setq quote-level (1+ quote-level))
             ;; escape quotes
             (when (>= quote-level 3)
               (setq quote-level 1)))
            ;; handle unquoted delimiters
            ((and (memq (char-after) separators) (/= quote-level 1))
             (setq found t)
             ;; reset quote level if quoted string has ended
             (when (>= quote-level 2)
               (setq quote-level 0)))
            ;; handle newlines
            ((eq (char-after) ?\n)
             (setq at-newline t
                   quote-level 0))
            ;; reset quote level if quoted string has ended
            ((>= quote-level 2)
             (setq quote-level 0)))
      (forward-char))
    (list found quote-level at-newline)))

(defun csv-rainbow--partial-parse (separators field-quotes &optional pos)
  (save-excursion
    (if pos
        (goto-char pos)
      (setq pos (point)))
    (beginning-of-line)
    (let ((col-no 0)
          (quote-level 0)
          (continue t))
      (while (and (<= (point) pos) (< (point) (point-max)) continue)
        (let* ((result (csv-rainbow--forward-delimiter separators field-quotes
                                                       pos quote-level))
               (delimiter-found (nth 0 result))
               (at-newline (nth 2 result)))
          (setq quote-level (nth 1 result))
          (setq col-no (1+ col-no))
          (when at-newline
            (setq col-no 0)
            (setq continue nil))))
      (list col-no quote-level))))

(defun csv-rainbow--col-matcher (separators field-quotes color-idx num-colors)
  (lambda (end)
    (let* ((partial-parse-result
            (csv-rainbow--partial-parse separators field-quotes))
           (col-no (car partial-parse-result))
           (quote-level (nth 1 partial-parse-result))
           (match-found nil))
      (while (and (<= (point) end) (< (point) (point-max)) (not match-found))
        (let* ((prev-pos (point))
               (move-result
                (csv-rainbow--forward-delimiter separators field-quotes end))
               (delimiter-found (nth 0 move-result))
               (at-newline (nth 2 move-result)))
          (when (= (% col-no num-colors) color-idx)
            (set-match-data (list prev-pos (1- (point))))
            (setq match-found t))

          (when delimiter-found
            (setq col-no (1+ col-no)))
          (when at-newline
            (setq col-no 1))))
      match-found)))

(defun csv-rainbow--create-font-lock-keywords (faces)
  (let ((separators csv-separator-chars)
        (field-quotes (mapcar #'string-to-char csv-field-quotes))
        (keywords nil))
    (dotimes (i (length faces))
      (push `(,(csv-rainbow--col-matcher separators field-quotes i
                                         (length faces))
              . ',(nth i faces))
            keywords))
    keywords))

;;;###autoload
(define-minor-mode csv-rainbow-mode
  "Toggle the display of colored columns in csv/tsv mode buffers."
  :lighter nil
  (unless (and (boundp 'csv-rainbow--faces) csv-rainbow--faces)
    (csv-rainbow--define-faces))
  (unless (and (boundp 'csv-rainbow--font-lock-keywords)
               csv-rainbow--font-lock-keywords)
    (setq csv-rainbow--font-lock-keywords
          (csv-rainbow--create-font-lock-keywords csv-rainbow--faces)))
  (unless (boundp 'csv-rainbow--prev-font-lock-defaults)
    (setq csv-rainbow--prev-font-lock-defaults font-lock-defaults))

  (font-lock-remove-keywords nil csv-rainbow--font-lock-keywords)
  (if csv-rainbow-mode
      (progn
        (setq font-lock-defaults nil)
        (font-lock-refresh-defaults)
        (setq font-lock-keywords-only t)
        (font-lock-add-keywords nil csv-rainbow--font-lock-keywords 'append)
;;        (set (make-local-variable 'jit-lock-contextually) nil)
        )
    (progn
      (setq font-lock-defaults csv-rainbow--prev-font-lock-defaults)
      (setq font-lock-keywords-only nil)
      (font-lock-refresh-defaults)))
  (when font-lock-mode
    (font-lock-fontify-buffer)))

(provide 'csv-rainbow)
;;; csv-rainbow.el ends here
