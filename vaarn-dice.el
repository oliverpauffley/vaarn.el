;;; vaarn-dice.el --- Dice rolling functionality -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Oliver Pauffley
;;
;; Author: Oliver Pauffley <mrpauffley@gmail.com>
;; Maintainer: Oliver Pauffley <mrpauffley@gmail.com>
;; Created: November 18, 2025
;; Modified: November 18, 2025
;; Version: 0.0.1
;; Homepage: https://github.com/oliverpauffley/vaarn.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'dash)


(defvar vaarn-dice-previous-roll-expression nil
  "Store the dice expression for later in case we want to re-roll.")

(defun vaarn-dice--d6 ()
  "Roll a 6 sided dice."
  (vaarn-dice--roll-die 6))

(defun vaarn-dice--roll-die (size)
  "Roll a SIZE sided dice."
  (1+ (random size)))

(defun vaarn-dice--roll-dice (count sides)
  "Get a random set of COUNT dice rolls where each die has SIDES."
  (let (value)
    (dotimes (_ count value)
      (setq value (cons (vaarn-dice--roll-die sides) value)))))

(defun vaarn-dice--roll (count size &optional modifier)
  "Get a random dice roll of COUNT number of SIZE sided dice.
where MODIFIER is a number to be added after the roll is done."
  (let* ((rolls (vaarn-dice--roll-dice count size)))
    (if (numberp modifier) (cons rolls modifier) (cons rolls 0))))

(defun vaarn-dice--result (roll-mod)
  "Get the sum of a tuple of ROLL-MOD.
The `car' is a list of rolls and the `cdr' is a modifier"
  (let ((rolls (car roll-mod))
        (mod (cdr roll-mod)))
    (+ (-sum rolls) mod)))

(defvar vaarn-dice-roll-regexp
  (rx word-start
      (optional (group (one-or-more digit)))
      "d"
      (group (one-or-more digit))
      (optional
       (group (or "+" "-"))
       (group (one-or-more digit)))
      (optional ":"
                (group (one-or-more digit)))
      word-end)
  "A regular expression that matches a dice roll.")

(defun vaarn-dice-forward-roll (count)
  "Move to the next COUNT of a dice roll."
  (interactive "p")
  (when (looking-at-p vaarn-dice-roll-regexp)
    (re-search-forward vaarn-dice-roll-regexp))
  (dotimes (repeat count)
    (re-search-forward vaarn-dice-roll-regexp))
  (goto-char (match-beginning 0)))

(defun vaarn-dice--roll-str (str)
  "Return a dice roll of STR."
  (if (string-match vaarn-dice-roll-regexp str)
      (let* ((num-dice-s  (or (match-string 1 str) "1"))
             (num-dice    (string-to-number num-dice-s))
             (dice-type-s (or (match-string 2 str) "20"))
             (dice-type   (string-to-number dice-type-s))
             (plus-minus  (or (match-string 3 str) "+"))
             (modifier-s  (or (match-string 4 str) "0"))
             (modifier-abs    (string-to-number modifier-s))
             (modifier (if (equal "-" plus-minus) (- modifier-abs) modifier-abs)))
        (vaarn-dice--roll num-dice dice-type modifier))
    (vaarn-dice--roll 1 20)))

(defun vaarn-dice-display (roll-mod &optional expression)
  "Convert a ROLL-MOD tuple into a display string.
The EXPRESSION is the string that generated the ROLL-MOD result."
  (let ((sum (vaarn-dice--result roll-mod))
        (rolls (car roll-mod))
        (modifier (cdr roll-mod)))
    (vaarn-dice--display-parts sum rolls modifier expression)))

(defface vaarn-dice-roll-expression
  '((t :inherit 'error))
  "Face for dice expression (if given)."
  :group 'vaarn)

(defface vaarn-dice-display-sequence
  '((t :inherit 'shadow))
  "Face for unimportant part of a displayed message."
  :group 'vaarn)

(defface vaarn-dice-display-mod
  '((t :inherit 'shadow))
  "Face for modifiers."
  :group 'vaarn)

(defun vaarn-dice--display-roll (roll)
  "convert a ROLL from a list of dice rolled integers to a string."
  (if roll
      (let ((roll-str (seq-map (lambda (e) (format "%s" e)) roll)))
        (concat "「" (string-join roll-str " ") "」"))
    " "))

(defun vaarn-dice--display-parts (sum rolls modifier &optional expression)
  "Display dice rolls as a string.
SUM is the total of ROLLS and MODIFIER.  The EXPRESSION is the
string used to generate the roll."
  (let* ((sum-str (propertize (number-to-string sum) 'face 'warning))
         (roll-str (propertize (vaarn-dice--display-roll rolls) 'face 'vaarn-dice-display-sequence))
         (sep (propertize " … " 'face 'vaarn-dice-display-sequence))
         (mod-str (cond ((equal 0 modifier) "")
                        ((> 0 modifier) (propertize (format "- %d" modifier) 'face 'vaarn-dice-display-mod))
                        (t (propertize (format "+ %d" modifier) 'face 'vaarn-dice-display-mod))))
         (expr-str (if expression
                       (format " | %s"
                               (propertize expression 'face 'vaarn-dice-roll-expression))
                     "")))
    (format "%s%s%s%s%s" sum-str sep roll-str mod-str expr-str)))


(defun vaarn-dice-roll (expression)
  "Generate a random number based on a give dice roll EXPRESSION.
Unless the point is on a dice roll description.  In that case
roll the description."
  (interactive (list (if (looking-at vaarn-dice-roll-regexp)
                         (match-string-no-properties 0)
                       (read-string "Dice Expression: "))))
  (setq vaarn-dice-previous-roll-expression expression)
  (let* ((rolls (vaarn-dice--roll-str expression)))
    (message "Rolled: %s"
             (vaarn-dice-display rolls expression))))

(defun vaarn-roll-again ()
  "Roll the last expression again."
  (interactive)
  (if vaarn-dice-previous-roll-expression
      (vaarn-dice-roll vaarn-dice-previous-roll-expression)
    (call-interactively 'vaarn-dice-roll)))

(provide 'vaarn-dice)
;;; Vaarn-dice.el ends here
