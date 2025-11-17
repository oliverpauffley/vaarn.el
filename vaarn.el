;;; vaarn.el --- Game master tools for vaults of vaarn -*- lexical-binding: t; -*-
;;;
;; Author: Oliver Pauffley <mrpauffley@gmail.com>
;; Maintainer: Oliver Pauffley <mrpauffley@gmail.com>
;; Created: October 28, 2025
;; Modified: October 28, 2025
;; Version: 0.0.1
;; Keywords: emulations games
;; Homepage: https://github.com/oliverpauffley/vaarn.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; A set of helper functions for vaults of vaarn
;;
;;
;;; Code:
(require 'cl-lib)
(require 'cube)
(require 'multisession)

(require 'vaarn-weather)

(defgroup vaarn nil
  "Tools for vaarn referees."
  :group 'tools
  :prefix "vaarn-")

(defun vaarn--d6 ()
  "Roll a 6 sided dice."
  (vaarn--roll-dice 6))

(defun vaarn--roll-dice (size)
  "Roll a SIZE sided dice."
  (+ (random size) 1))

(defun vaarn--visible-buffer-p (buf)
  "Return non-nil if BUF is visible."
  (get-buffer-window buf))

(provide 'vaarn)
;;; vaarn.el ends here
