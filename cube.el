;;; cube.el --- cube coordinates for hexagonal grids -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Oliver Pauffley
;;
;; Author: Oliver Pauffley <mrpauffley@gmail.com>
;; Maintainer: Oliver Pauffley <mrpauffley@gmail.com>
;; Created: October 28, 2025
;; Modified: October 28, 2025
;; Version: 0.0.1
;; Keywords: data games
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Cube based coordinate system for hexagonal grids
;; more information here:
;;
;;; Code:

(require 'cl-lib)

(cl-defstruct cube-coord
  "Using a cube coordinate system.
Q is zero from top to bottom, or q is the horizonal axis
R is zero from top left to bottom right
S is zero from bottom left to top right.
see https://www.redblobgames.com/grids/hexagons/#coordinates-cube ."
  (q 0 :type int)
  (r 0 :type int)
  (s 0 :type int))

(defun cube-coord-move-up (c)
  "Move north/up."
  (cl-decf (cube-coord-r c))
  (cl-incf (cube-coord-s c))
  c)

(message "%s" (cube-coord-move-up (make-cube-coord :q 1 :r 1 :s 1)))

(provide 'cube)
;;; cube.el ends here
