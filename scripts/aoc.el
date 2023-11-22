;;; aoc.el --- Description -*- lexical-binding: t; -*-
;;
;; Package-Requires: ((emacs "29.1"))
;;
;;; Commentary:
;; Helpers for working with Advent of Code puzzles
;;
;;; Code:

(require 'dash)
(require 'projectile)
(require 's)

(defun aoc-split-filename ()
  (pcase-let ((`(,ext ,day ,year) (reverse (s-split "[\\./]" (buffer-file-name)))))
    (list year (s-chop-left 3 (s-chop-prefix "test_" day)) ext)))

(defun aoc-run-command (args)
  (projectile-run-async-shell-command-in-root (s-join " " (cons "./aoc.sh" args)) (get-buffer-create "*AoC*")))

(defun aoc-run-buffer-command (cmd)
  (aoc-run-command (cons cmd (aoc-split-filename))))

(defun aoc-find-resource (path)
  (find-file (concat (projectile-acquire-root) "/resources/"  path)))

(defun aoc-open-input ()
  (interactive)
  (aoc-find-resource (-interleave (aoc-split-filename) '("/day" ".txt"))))

(defun aoc-open-solution ()
  (interactive)
  (aoc-find-resource (-interleave (aoc-split-filename) '("/solutions/day" ".txt"))))

(defun aoc-run-tests ()
  (interactive)
  (aoc-run-buffer-command "test"))

(define-minor-mode aoc-mode
  nil
  :keymap (map! (:localleader
                 (:map (aoc-mode-map)
                       (:prefix ("a" . "")
                                "s" #'aoc-open-solution
                                "t" #'aoc-run-tests)))))

(provide 'aoc)

;;; aoc.el ends here
