;;; theme-switch.el --- Theme switching with eye-care mode and favorites -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 tmhourglass

;; Author: tmhourglass
;; Keywords: convenience, themes
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the following features:
;; 1. Maintains a list of favorite themes
;; 2. Random theme switching with the ability to add to favorites
;; 3. Time-based eye-care mode (day/night mode) with one-click toggle
;; 4. Theme preview using consult-theme

;;; Code:

(require 'cl-lib)

;;; Custom options

(defgroup theme-switch nil
  "Theme switching functionality with eye-care mode."
  :group 'convenience
  :prefix "theme-switch-")

(defcustom theme-switch-favorite-themes nil
  "List of favorite themes.
If empty, all available themes will be used."
  :type '(repeat symbol)
  :group 'theme-switch)

(defcustom theme-switch-excluded-themes '(user)
  "List of themes to exclude.
These themes will not be selected randomly."
  :type '(repeat symbol)
  :group 'theme-switch)

(defcustom theme-switch-day-themes nil
  "List of themes for day mode.
If empty, a random theme from favorites will be selected."
  :type '(repeat symbol)
  :group 'theme-switch)

(defcustom theme-switch-night-themes nil
  "List of themes for night mode.
If empty, a random theme from favorites will be selected."
  :type '(repeat symbol)
  :group 'theme-switch)

(defcustom theme-switch-day-start "06:00"
  "Start time for day mode, format is \"HH:MM\"."
  :type 'string
  :group 'theme-switch)

(defcustom theme-switch-night-start "18:00"
  "Start time for night mode, format is \"HH:MM\"."
  :type 'string
  :group 'theme-switch)

(defcustom theme-switch-auto-switch-enabled nil
  "Whether to enable automatic theme switching based on time."
  :type 'boolean
  :group 'theme-switch)

(defcustom theme-switch-auto-switch-interval 3600
  "Interval in seconds to check if theme should be switched."
  :type 'integer
  :group 'theme-switch)

;;; Internal variables

(defvar theme-switch--current-theme nil
  "Currently active theme.")

(defvar theme-switch--timer nil
  "Timer for automatic theme switching.")

(defvar theme-switch--history nil
  "History of used themes.")

(defvar theme-switch--eye-care-mode nil
  "Current state of eye-care mode: 'day, 'night, or nil (off).")

;;; Hooks

(defvar theme-switch-before-load-theme-hook nil
  "Hook run before loading a theme.
Functions receive one argument: the theme name (symbol).")

(defvar theme-switch-after-load-theme-hook nil
  "Hook run after loading a theme.
Functions receive one argument: the theme name (symbol).")

;;; Utility functions

(defun theme-switch--disable-all-themes ()
  "Disable all currently enabled themes."
  (mapc #'disable-theme custom-enabled-themes))

(defun theme-switch--get-favorite-themes ()
  "Get the list of favorite themes.
If `theme-switch-favorite-themes' is empty, return all available themes
except those in `theme-switch-excluded-themes'."
  (if theme-switch-favorite-themes
      theme-switch-favorite-themes
    (cl-set-difference (custom-available-themes) theme-switch-excluded-themes)))

(defun theme-switch--get-available-themes ()
  "Get the list of all available themes.
  Except those in `theme-switch-excluded-themes'."
  (cl-set-difference (custom-available-themes) theme-switch-excluded-themes)

(defun theme-switch--random-theme (themes)
  "Select a random theme from THEMES."
  (nth (random (length themes)) themes))

(defun theme-switch--time-string-to-minutes (time-str)
  "Convert time string TIME-STR in format \"HH:MM\" to minutes."
  (let ((hour (string-to-number (substring time-str 0 2)))
        (minute (string-to-number (substring time-str 3 5))))
    (+ (* hour 60) minute)))

(defun theme-switch--current-time-in-minutes ()
  "Get current time in minutes."
  (let ((time (decode-time)))
    (+ (* (nth 2 time) 60) (nth 1 time))))

(defun theme-switch--is-day-time-p ()
  "Determine if current time is day time."
  (let* ((current (theme-switch--current-time-in-minutes))
         (day-start (theme-switch--time-string-to-minutes theme-switch-day-start))
         (night-start (theme-switch--time-string-to-minutes theme-switch-night-start)))
    (if (< day-start night-start)
        ;; Simple case: day before night
        (and (>= current day-start) (< current night-start))
      ;; Complex case: day spans midnight
      (or (>= current day-start) (< current night-start)))))

;;; Main functions

(defun theme-switch-load-theme (theme)
  "Load the specified THEME."
  (interactive
   (list (intern (completing-read "Select theme: "
                                 (mapcar #'symbol-name (theme-switch--get-favorite-themes))
                                 nil t))))
  (when theme
    ;; Run pre-load hooks
    (run-hook-with-args 'theme-switch-before-load-theme-hook theme)

    ;; Disable current theme and load new one
    (theme-switch--disable-all-themes)
    (load-theme theme t)

    ;; Update state
    (setq theme-switch--current-theme theme)
    (push theme theme-switch--history)

    ;; Run post-load hooks
    (run-hook-with-args 'theme-switch-after-load-theme-hook theme)

    (message "Loaded theme: %s" theme)))

(defun theme-switch-random-favorites ()
  "Load a random theme from favorites list.
If favorites list is empty, use all available themes except excluded ones."
  (interactive)
  (let* ((themes (theme-switch--get-favorite-themes))
         (theme (theme-switch--random-theme themes)))
    (theme-switch-load-theme theme)))

(defun theme-switch-random-available ()
  "Load a random theme from all available themes.
This ignores excluded lists."
  (interactive)
  (let* ((themes (theme-switch--get-available-themes))
         (theme (theme-switch--random-theme themes)))
    (theme-switch-load-theme theme)))

(defun theme-switch-add-to-favorites (&optional theme)
  "Add THEME to favorite themes list.
If THEME is nil, add the current theme."
  (interactive)
  (let ((theme-to-add (or theme
                          theme-switch--current-theme
                          (intern (completing-read "Select theme to add to favorites: "
                                                 (mapcar #'symbol-name (custom-available-themes))
                                                 nil t)))))
    (unless (memq theme-to-add theme-switch-favorite-themes)
      (customize-save-variable 'theme-switch-favorite-themes
                             (cons theme-to-add theme-switch-favorite-themes))
      (message "Added %s to favorite themes" theme-to-add))))

(defun theme-switch-remove-from-favorites (&optional theme)
  "Remove THEME from favorite themes list.
If THEME is nil, remove the current theme or prompt for a theme."
  (interactive)
  (let ((theme-to-remove (or theme
                             theme-switch--current-theme
                             (intern (completing-read "Select theme to remove from favorites: "
                                                    (mapcar #'symbol-name theme-switch-favorite-themes)
                                                    nil t)))))
    (when (memq theme-to-remove theme-switch-favorite-themes)
      (customize-save-variable 'theme-switch-favorite-themes
                             (remove theme-to-remove theme-switch-favorite-themes))
      (message "Removed %s from favorite themes" theme-to-remove))))

(defun theme-switch-add-to-excluded (&optional theme)
  "Add THEME to excluded themes list.
If THEME is nil, prompt for a theme."
  (interactive)
  (let ((theme-to-add (or theme
                          (intern (completing-read "Select theme to add to excluded: "
                                                 (mapcar #'symbol-name (custom-available-themes))
                                                 nil t)))))
    (unless (memq theme-to-add theme-switch-excluded-themes)
      (customize-save-variable 'theme-switch-excluded-themes
                             (cons theme-to-add theme-switch-excluded-themes))
      (message "Added %s to excluded themes" theme-to-add))))

(defun theme-switch-remove-from-excluded (&optional theme)
  "Remove THEME from excluded themes list.
If THEME is nil, prompt for a theme."
  (interactive)
  (let ((theme-to-remove (or theme
                             (intern (completing-read "Select theme to remove from excluded: "
                                                    (mapcar #'symbol-name theme-switch-excluded-themes)
                                                    nil t)))))
    (when (memq theme-to-remove theme-switch-excluded-themes)
      (customize-save-variable 'theme-switch-excluded-themes
                             (remove theme-to-remove theme-switch-excluded-themes))
      (message "Removed %s from excluded themes" theme-to-remove))))

(defun theme-switch-list-favorites ()
  "List all favorite themes."
  (interactive)
  (if theme-switch-favorite-themes
      (message "Favorite themes: %s"
               (mapconcat #'symbol-name theme-switch-favorite-themes ", "))
    (message "No favorite themes set. Using all available themes except excluded ones.")))

(defun theme-switch-list-excluded ()
  "List all excluded themes."
  (interactive)
  (if theme-switch-excluded-themes
      (message "Excluded themes: %s"
               (mapconcat #'symbol-name theme-switch-excluded-themes ", "))
    (message "No excluded themes set.")))

(defun theme-switch-day-mode ()
  "Switch to day mode."
  (interactive)
  (let* ((themes (if theme-switch-day-themes
                    theme-switch-day-themes
                  (theme-switch--get-favorite-themes)))
         (theme (theme-switch--random-theme themes)))
    (theme-switch-load-theme theme)
    (setq theme-switch--eye-care-mode 'day)
    (message "Switched to day mode: %s" theme)))

(defun theme-switch-night-mode ()
  "Switch to night mode."
  (interactive)
  (let* ((themes (if theme-switch-night-themes
                    theme-switch-night-themes
                  (theme-switch--get-favorite-themes)))
         (theme (theme-switch--random-theme themes)))
    (theme-switch-load-theme theme)
    (setq theme-switch--eye-care-mode 'night)
    (message "Switched to night mode: %s" theme)))

(defun theme-switch-auto ()
  "Automatically switch theme based on current time."
  (interactive)
  (if (theme-switch--is-day-time-p)
      (theme-switch-day-mode)
    (theme-switch-night-mode)))

(defun theme-switch-previous ()
  "Switch to the previously used theme."
  (interactive)
  (when (> (length theme-switch--history) 1)
    ;; Current theme should be the first in history
    (let ((current (car theme-switch--history))
          (previous (cadr theme-switch--history)))
      ;; Update history, move current theme to the back
      (setq theme-switch--history (cdr theme-switch--history))
      (setq theme-switch--history (nconc theme-switch--history (list current)))
      ;; Load previous theme
      (theme-switch--disable-all-themes)
      (load-theme previous t)
      (setq theme-switch--current-theme previous)
      (message "Switched to previous theme: %s" previous))))

(defun theme-switch-toggle-auto-switch ()
  "Toggle automatic theme switching."
  (interactive)
  (setq theme-switch-auto-switch-enabled (not theme-switch-auto-switch-enabled))
  (theme-switch--setup-timer)
  (message "Automatic theme switching %s" (if theme-switch-auto-switch-enabled "enabled" "disabled")))

(defun theme-switch-toggle-eye-care ()
  "Toggle eye-care mode between day, night, and off."
  (interactive)
  (cond
   ((eq theme-switch--eye-care-mode 'day)
    (theme-switch-night-mode))
   ((eq theme-switch--eye-care-mode 'night)
    (setq theme-switch--eye-care-mode nil)
    (message "Eye-care mode disabled"))
   (t
    (theme-switch-day-mode))))

(defun theme-switch--setup-timer ()
  "Set up or cancel the automatic theme switching timer."
  (when theme-switch--timer
    (cancel-timer theme-switch--timer)
    (setq theme-switch--timer nil))

  (when theme-switch-auto-switch-enabled
    (setq theme-switch--timer
          (run-with-timer 0 theme-switch-auto-switch-interval 'theme-switch-auto))))

;;; Integration with consult-theme

(defun theme-switch-preview ()
  "Preview themes using consult-theme."
  (interactive)
  (if (fboundp 'consult-theme)
      (call-interactively 'consult-theme)
    (message "consult-theme is not available. Please install consult package.")))

;;; Command menu

(defvar theme-switch-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'theme-switch-load-theme)
    (define-key map (kbd "r") 'theme-switch-random-favorites)
    (define-key map (kbd "R") 'theme-switch-random-available)
    (define-key map (kbd "p") 'theme-switch-previous)
    (define-key map (kbd "d") 'theme-switch-day-mode)
    (define-key map (kbd "n") 'theme-switch-night-mode)
    (define-key map (kbd "e") 'theme-switch-toggle-eye-care)
    (define-key map (kbd "a") 'theme-switch-auto)
    (define-key map (kbd "t") 'theme-switch-toggle-auto-switch)
    (define-key map (kbd "v") 'theme-switch-preview)
    (define-key map (kbd "+") 'theme-switch-add-to-favorites)
    (define-key map (kbd "-") 'theme-switch-remove-from-favorites)
    (define-key map (kbd "x+") 'theme-switch-add-to-excluded)
    (define-key map (kbd "x-") 'theme-switch-remove-from-excluded)
    (define-key map (kbd "f") 'theme-switch-list-favorites)
    (define-key map (kbd "x") 'theme-switch-list-excluded)
    map)
  "Keymap for theme switching commands.")

(defun theme-switch-menu ()
  "Display menu of theme switching commands."
  (interactive)
  (let ((choice (read-char-choice
                "Theme Switching Commands:
l: Load theme
r: Random theme from favorites
R: Random theme from all available
p: Previous theme
d: Day mode
n: Night mode
e: Toggle eye-care mode
a: Auto switch
t: Toggle auto mode
v: Preview themes
+: Add current to favorites
-: Remove from favorites
f: List favorites
x: List excluded
q: Quit
"
                '(?l ?r ?R ?p ?d ?n ?e ?a ?t ?v ?+ ?- ?f ?x ?q))))
    (unless (eq choice ?q)
      (let ((cmd (lookup-key theme-switch-command-map (char-to-string choice))))
        (when cmd
          (call-interactively cmd))))))

;;;###autoload
(define-minor-mode theme-switch-mode
  "Theme switching mode with eye-care and favorites."
  :global t
  :lighter " ThemeSwitch"
  (if theme-switch-mode
      (progn
        (theme-switch--setup-timer)
        (message "Theme switching mode enabled"))
    (when theme-switch--timer
      (cancel-timer theme-switch--timer)
      (setq theme-switch--timer nil))
    (message "Theme switching mode disabled")))

(provide 'theme-switch)

;;; theme-switch.el ends here
