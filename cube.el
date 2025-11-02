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
(require 'ert)


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


(defconst cube--test-origin (make-cube-coord :q 0 :r 0 :s 0))

(ert-deftest cube-coord-move-n-test ()
  "Test moving north decreases r by 1 and increases s by 1."
  (let ((result (cube-coord-move-n cube--test-origin)))
    (should (equal result (make-cube-coord :q 0 :r -1 :s 1)))))

(ert-deftest cube-coord-move-s-test ()
  "Test moving south increases r by 1 and decreases s by 1."
  (let ((result (cube-coord-move-s cube--test-origin)))
    (should (equal result (make-cube-coord :q 0 :r 1 :s -1)))))

(ert-deftest cube-coord-move-ne-test ()
  "Test moving north-east decreases q by 1 and increases s by 1."
  (let ((result (cube-coord-move-ne cube--test-origin)))
    (should (equal result (make-cube-coord :q -1 :r 0 :s 1)))))

(ert-deftest cube-coord-move-sw-test ()
  "Test moving south-west increases q by 1 and decreases s by 1."
  (let ((result (cube-coord-move-sw cube--test-origin)))
    (should (equal result (make-cube-coord :q 1 :r 0 :s -1)))))

(ert-deftest cube-coord-move-nw-test ()
  "Test moving north-west increases q by 1 and decreases r by 1."
  (let ((result (cube-coord-move-nw cube--test-origin)))
    (should (equal result (make-cube-coord :q 1 :r -1 :s 0)))))

(ert-deftest cube-coord-move-se-test ()
  "Test moving south-east decreases q by 1 and increases r by 1."
  (let ((result (cube-coord-move-se cube--test-origin)))
    (should (equal result (make-cube-coord :q -1 :r 1 :s 0)))))

(ert-deftest cube-coord-move-n-s-composition-test ()
  "Moving north then south returns to origin."
  (let* ((north (cube-coord-move-n cube--test-origin))
         (back (cube-coord-move-s north)))
    (should (equal back cube--test-origin))))

(ert-deftest cube-coord-move-s-n-composition-test ()
  "Moving south then north returns to origin."
  (let* ((south (cube-coord-move-s cube--test-origin))
         (back (cube-coord-move-n south)))
    (should (equal back cube--test-origin))))

(ert-deftest cube-coord-move-ne-sw-composition-test ()
  "Moving north-east then south-west returns to origin."
  (let* ((ne (cube-coord-move-ne cube--test-origin))
         (back (cube-coord-move-sw ne)))
    (should (equal back cube--test-origin))))

(ert-deftest cube-coord-move-sw-ne-composition-test ()
  "Moving south-west then north-east returns to origin."
  (let* ((sw (cube-coord-move-sw cube--test-origin))
         (back (cube-coord-move-ne sw)))
    (should (equal back cube--test-origin))))

(ert-deftest cube-coord-move-nw-se-composition-test ()
  "Moving north-west then south-east returns to origin."
  (let* ((nw (cube-coord-move-nw cube--test-origin))
         (back (cube-coord-move-se nw)))
    (should (equal back cube--test-origin))))

(ert-deftest cube-coord-move-se-nw-composition-test ()
  "Moving south-east then north-west returns to origin."
  (let* ((se (cube-coord-move-se cube--test-origin))
         (back (cube-coord-move-nw se)))
    (should (equal back cube--test-origin))))

(defun cube-coord-add (a b)
  "Add A and B cube coords."
  (make-cube-coord
   :q (+ (cube-coord-q a) (cube-coord-q b))
   :r (+ (cube-coord-r a) (cube-coord-r b))
   :s (+ (cube-coord-s a) (cube-coord-s b))))

(ert-deftest cube-coord-add ()
  "Tests adding cube coordinates together."
  (should (equal (cube-coord-add (cube-co 0 0 0) (cube-co 0 0 0)) (cube-co 0 0 0)))
  (should (equal (cube-coord-add (cube-co 0 0 0) (cube-co 1 -1 0)) (cube-co 1 -1 0)))
  (should (equal (cube-coord-add (cube-co -3 1 2) (cube-co 3 -1 -2)) (cube-co 0 0 0)))
  (should (equal (cube-coord-add (cube-co 4 -2 -2) (cube-co 4 0 -4)) (cube-co 8 -2 -6))))


(defun cube-coord-flip-not-q (c)
  "For the coord C keep the q value the same but switch the r and s."
  (make-cube-coord :q (cube-coord-q c)
                   :r (cube-coord-s c)
                   :s (cube-coord-r c)))

(ert-deftest cube-coord-flip-not-q ()
  "Should keep the q coordinate the same whilst swapping the other two."
  (should (equal (cube-coord-flip-not-q (cube-co -1 -3 4)) (cube-co -1 4 -3)))
  (should (equal (cube-coord-flip-not-q (cube-co 0 1 -1)) (cube-co 0 -1 1))))

(defun cube-coord-flip-not-r (c)
  "For the coord C keep the r value the same but switch the q and s."
  (make-cube-coord :q (cube-coord-s c)
                   :r (cube-coord-r c)
                   :s (cube-coord-q c)))

(ert-deftest cube-coord-flip-not-r ()
  "Should keep the r coordinate the same whilst swapping the other two."
  (should (equal (cube-coord-flip-not-r (cube-co -1 -3 4)) (cube-co 4 -3 -1)))
  (should (equal (cube-coord-flip-not-r (cube-co 0 1 -1)) (cube-co -1 1 0))))

(defun cube-coord-flip-not-s (c)
  "For the coord C keep the s value the same but switch the q and r."
  (make-cube-coord :q (cube-coord-r c)
                   :r (cube-coord-q c)
                   :s (cube-coord-s c)))

(ert-deftest cube-coord-flip-not-s ()
  "Should keep the s coordinate the same whilst swapping the other two."
  (should (equal (cube-coord-flip-not-s (cube-co -1 -3 4)) (cube-co -3 -1 4)))
  (should (equal (cube-coord-flip-not-s (cube-co 0 1 -1)) (cube-co 1 0 -1))))

(provide 'cube)
;;; cube.el ends here
