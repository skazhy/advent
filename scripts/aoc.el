;;; aoc.el --- Description -*- lexical-binding: t; -*-
;;
;; Package-Requires: ((emacs "29.1"))
;;
;;; Commentary:
;; Helpers for working with Advent of Code puzzles
;;
;;; Code:

(require 'dash)
(require 'general)
(require 'projectile)
(require 's)

(defun aoc-split-filename ()
  (pcase-let ((`(,ext ,day ,year) (reverse (s-split "[\\./]" (buffer-file-name)))))
    (list year (s-chop-left 3 (s-chop-prefix "test_" day)) ext)))

(defun aoc-run-async-command (args)
  (projectile-run-async-shell-command-in-root (s-join " " (cons "./aoc.sh" args)) (get-buffer-create "*AoC*")))

(defun aoc-run-buffer-command (cmd)
  (aoc-run-async-command (cons cmd (aoc-split-filename))))

(defun aoc-run-str-command (cmd)
  (projectile-with-default-dir (projectile-acquire-root)
    (s-chop-suffix "\n" (shell-command-to-string (concat "./aoc.sh " cmd)))))

(defun aoc-find-resource (path)
  (find-file (concat (projectile-acquire-root) "/resources/" path)))

(defun aoc-open-input ()
  (interactive)
  (aoc-find-resource (apply 'concat (-interleave (aoc-split-filename) '("/day" ".txt")))))

(defun aoc-open-solution ()
  (interactive)
  (aoc-find-resource (apply 'concat (-interleave (aoc-split-filename) '("/solutions/day" ".txt")))))

(defun aoc-run-tests ()
  (interactive)
  (aoc-run-buffer-command "test"))

(defun aoc-find-or-create (arg)
  (interactive "sFind or create puzzle: ")
  (let ((src-path (concat (projectile-acquire-root) (aoc-run-str-command (concat arg " srcpath")))))
    (if (file-exists-p src-path)
        (find-file src-path)
      (message "Creating source for new puzzle...")
      (aoc-run-str-command (concat arg " setup"))
      (find-file src-path))))

(define-minor-mode aoc-mode
  nil
  :keymap (general-define-key :states 'normal
                              :prefix "SPC m"
                              :keymaps '(aoc-mode-map)
                              :infix "a" "" (list :ignore t)
                              "i" #'aoc-open-input
                              "o" #'aoc-find-or-create
                              "s" #'aoc-open-solution
                              "t" #'aoc-run-tests))

(provide 'aoc)

;;; aoc.el ends here
