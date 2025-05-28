;; -*- lexical-binding: t; -*-

;;; theme-timer.el --- put your themes on day/night cycles

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

(defgroup theme-timer nil
  "Time based theme switching."
  :prefix "theme-timer-")

(defcustom theme-timer-day-time-theme nil
  "The theme to use during daytime."
  :type 'symbol
  :group 'theme-timer)

(defcustom theme-timer-night-time-theme nil
  "The theme to use during the night."
  :type 'symbol
  :group 'theme-timer)

(defcustom theme-timer-day-time-hour 6
  "The hour [0-23] at which it becomes daytime."
  :type 'natnum
  :group 'theme-timer)

(defcustom theme-timer-night-time-hour 20
  "The hour [0-23] at which it becomes night."
  :type 'natnum
  :group 'theme-timer)

(defcustom theme-timer-change-hook nil
  "Runs whenever a theme is disabled, enabled, or swapped."
  :type 'hook
  :group 'theme-timer)

(defvar theme-timer--timer nil
  "The timer that calls the init function, which updates the theme if needed and
resets the timer to the appropriate hour.")

(defun theme-timer-use-theme (target other)
  "Disables the theme OTHER if enabled, and enables or loads TARGET if it's not
enabled or loaded. nil values are ignored. Returns non-nil if any change to the
current theme was made."
  (let ((dirty nil))
    (when (and other (memq other custom-enabled-themes))
      (disable-theme other)
      (setq dirty t))
    (when (and target (not (memq target custom-enabled-themes)))
      (if (custom-theme-p target)
          (enable-theme target)
        (load-theme target t))
      (setq dirty t))
    (when dirty
      (run-hooks 'theme-timer-change-hook))
    dirty))

(defun theme-timer-use-time-appropriate-theme ()
  (interactive)
  (let* ((decoded-current-time (decode-time (current-time)))
         (current-hour (decoded-time-hour decoded-current-time))
         (is-night (if (> theme-timer-night-time-hour theme-timer-day-time-hour)
                       (or (>= current-hour theme-timer-night-time-hour)
                           (< current-hour theme-timer-day-time-hour))
                     (and (>= current-hour theme-timer-night-time-hour)
                          (< current-hour theme-timer-day-time-hour)))))
    (if is-night
        (theme-timer-use-theme theme-timer-night-time-theme
                               theme-timer-day-time-theme)
      (theme-timer-use-theme theme-timer-day-time-theme
                             theme-timer-night-time-theme))
    is-night))

(defun theme-timer-init ()
  (theme-timer-use-time-appropriate-theme)
  (when theme-timer--timer
    (cancel-timer theme-timer--timer))

  (let* ((is-night (theme-timer-use-time-appropriate-theme))
         (next-hour (if is-night
                        theme-timer-day-time-hour
                      theme-timer-night-time-hour))
         (decoded-current-time (decode-time (current-time)))
         (current-hour (decoded-time-hour decoded-current-time))
         (current-minutes-and-seconds ; in seconds
          (+ (* 60 (decoded-time-minute decoded-current-time))
             (decoded-time-second decoded-current-time)))
         (num-hours (if (> next-hour current-hour)
                        (- next-hour current-hour)
                      (+ (- 24 current-hour) next-hour)))
         (num-seconds (- (* 60 60 num-hours) current-minutes-and-seconds)))
    (setq theme-timer--timer (run-with-timer num-seconds nil #'theme-timer-init))
    theme-timer--timer))

(provide 'theme-timer)
