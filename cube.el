;;; cube.el --- Cube coordinates for hexagonal grids -*- lexical-binding: t; -*-
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
;; more information here: https://www.redblobgames.com/grids/hexagons/#coordinates-cube."
;;
;;; Code:

(require 'cl-lib)

(cl-defstruct cube-coord
  "Using a cube coordinate system.
Q is zero from top to bottom, or q is the horizonal axis
R is zero from top left to bottom right
S is zero from bottom left to top right.
see https://www.redblobgames.com/grids/hexagons/#coordinates-cube.
Each set of coordinates should always sum to zero."
  (q 0 :type int)
  (r 0 :type int)
  (s 0 :type int))

(defun cube-co (q r s)
  "Shorthand constructor for `cube-coord'."
  (cl-assert (= 0 (+ q r s)))
  (make-cube-coord :q q :r r :s s))

(defun cube-coord-move-n (c)
  "Move coord C north/up."
  (cube-coord-add c (make-cube-coord :q 0 :r -1 :s 1)))

(defun cube-coord-move-s (c)
  "Move coord C south/down."
  (cube-coord-add c (make-cube-coord :q 0 :r 1 :s -1)))

(defun cube-coord-move-ne (c)
  "Move coord C north east."
  (cube-coord-add c (make-cube-coord :q -1 :r 0 :s 1)))

(defun cube-coord-move-sw (c)
  "Move coord C south west."
  (cube-coord-add c (make-cube-coord :q 1 :r 0 :s -1)))

(defun cube-coord-move-nw (c)
  "Move coord C north west."
  (cube-coord-add c (make-cube-coord :q 1 :r -1 :s 0)))

(defun cube-coord-move-se (c)
  "Move coord C north east."
  (cube-coord-add c (make-cube-coord :q -1 :r 1 :s 0)))

(defun cube-coord-add (a b)
  "Add A and B cube coords."
  (make-cube-coord
   :q (+ (cube-coord-q a) (cube-coord-q b))
   :r (+ (cube-coord-r a) (cube-coord-r b))
   :s (+ (cube-coord-s a) (cube-coord-s b))))

(ert-deftest cube-coord-add ()
  "Tests adding cube coordinates together."
  (should (cube-eq (cube-coord-add (cube-co 0 0 0) (cube-co 0 0 0)) (cube-co 0 0 0)))
  (should (cube-eq (cube-coord-add (cube-co 0 0 0) (cube-co 1 -1 0)) (cube-co 1 -1 0)))
  (should (cube-eq (cube-coord-add (cube-co -3 1 2) (cube-co 3 -1 -2)) (cube-co 0 0 0)))
  (should (cube-eq (cube-coord-add (cube-co 4 -2 -2) (cube-co 4 0 -4)) (cube-co 8 -2 -6))))

(defun cube-eq (a b)
  "Return non nil if A and B are equal."
  (and
   (= (cube-coord-q a) (cube-coord-q b))
   (= (cube-coord-r a) (cube-coord-r b))
   (= (cube-coord-s a) (cube-coord-s b))))

(ert-deftest cube-co ()
  "Confirm equality."
  (should (cube-eq (cube-co -1 -3 4) (cube-co -1 -3 4)))
  (should (cube-eq (cube-co 1 -1 0) (cube-co 1 -1 0)))
  (should (cube-eq (cube-co -9 6 3) (cube-co -9 6 3))))

(defun cube-coord-flip-not-q (c)
  "For the coord C keep the q value the same but switch the r and s."
  (make-cube-coord :q (cube-coord-q c)
                   :r (cube-coord-s c)
                   :s (cube-coord-r c)))

(defun cube-coord-flip-not-s (c)
  "For the coord C keep the s value the same but switch the q and r."
  (make-cube-coord :q (cube-coord-r c)
                   :r (cube-coord-q c)
                   :s (cube-coord-s c)))

(defun cube-coord-flip-not-r (c)
  "For the coord C keep the r value the same but switch the q and s."
  (make-cube-coord :q (cube-coord-s c)
                   :r (cube-coord-r c)
                   :s (cube-coord-q c)))

(provide 'cube)
;;; cube.el ends here
