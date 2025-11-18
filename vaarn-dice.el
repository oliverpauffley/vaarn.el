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

(defun vaarn-dice--d6 ()
  "Roll a 6 sided dice."
  (vaarn-dice--roll 6))

(defun vaarn-dice--roll (size)
  "Roll a SIZE sided dice."
  (+ (random size) 1))


(provide 'vaarn-dice)
;;; vaarn-dice.el ends here
