;;; aoc.el --- Description -*- lexical-binding: t; -*-
;;
;; Package-Requires: ((emacs "29.1"))
;;
;;; Commentary:
;; Helpers for working with Advent of Code puzzles
;;
;;; Code:

(require 'general)
(require 'projectile)
(require 's)

(defvar puzzle-day)
(defvar puzzle-year)
(defvar puzzle-ext)
(defvar puzzle-src-buffer)
(defvar puzzle-test-buffer)

(defun aoc-run-async-command (args)
  (projectile-run-async-shell-command-in-root (s-join " " (cons "./aoc.sh" args)) (get-buffer-create "*AoC*")))

(defun aoc-run-buffer-command (cmd)
  (aoc-run-async-command (list puzzle-ext puzzle-year puzzle-day cmd)))

(defun aoc-run-str-command (cmd)
  (projectile-with-default-dir (projectile-acquire-root)
    (s-chop-suffix "\n" (shell-command-to-string (concat "./aoc.sh " cmd)))))

(defun aoc-find-file (path)
  (find-file (concat (projectile-acquire-root) path)))

(defun aoc-open-input ()
  (interactive)
  (when (boundp 'puzzle-year)
    (aoc-find-file (concat "/resources/" puzzle-year "/day" puzzle-day ".txt"))))

(defun aoc-open-solution ()
  (interactive)
  (when (boundp 'puzzle-year)
    (aoc-find-file (concat "/resources/" puzzle-year "/solutions/day" puzzle-day ".txt"))))

(defun aoc-lint ()
  (interactive)
  (aoc-run-buffer-command "lint"))

(defun aoc-test ()
  (interactive)
  (aoc-run-buffer-command "test"))

(defun aoc-run ()
  (interactive)
  (aoc-run-buffer-command "run"))

(defun aoc-switch-test-src ()
  (interactive)
  (cond
   ((and (eq major-mode 'clojure-mode) (bound-and-true-p puzzle-test-buffer))
    (aoc-find-file (concat "/src/advent/" puzzle-year "/day" puzzle-day ".clj")))
   ((and (eq major-mode 'clojure-mode) (bound-and-true-p puzzle-src-buffer))
    (aoc-find-file (concat "/test/advent/" puzzle-year "/test_day" puzzle-day ".clj")))))

(defun aoc-browse ()
  (interactive)
  (browse-url
   (concat "https://adventofcode.com/"
           (cond
            ((boundp 'puzzle-year) (concat puzzle-year "/day/" puzzle-day))
            ((string= "12" (format-time-string "%m")) (format-time-string "%Y"))
            (t (number-to-string (- (string-to-number (format-time-string "%Y")) 1)))))))

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
                              "b" #'aoc-browse
                              "i" #'aoc-open-input
                              "l" #'aoc-lint
                              "o" #'aoc-find-or-create
                              "s" #'aoc-open-solution
                              "t" #'aoc-test
                              "T" #'aoc-switch-test-src)
  (when (and aoc-mode (buffer-file-name))
    ;; '("clj" "day3" "2021" "advent" "src")
    (let ((split-path (reverse (s-split "[\\./]" (buffer-file-name)))))
      (when (or (string= "src" (nth 4 split-path)) (string= "test" (nth 4 split-path)))
        (setq-local puzzle-day (s-chop-left 3 (s-chop-prefix "test_" (nth 1 split-path))))
        (setq-local puzzle-year (s-chop-prefix "year" (nth 2 split-path)))
        (setq-local puzzle-ext (car split-path))
        (setq-local puzzle-src-buffer (string= "src" (nth 4 split-path)))
        (setq-local puzzle-test-buffer (string= "test" (nth 4 split-path)))))))

(provide 'aoc)

;;; aoc.el ends here
