;;; vaarn.el --- Game master tools for vaults of vaarn -*- lexical-binding: t; -*-
;;;
;; Copyright (C) 2025 Oliver Pauffley
;;
;; Author: Oliver Pauffley <mrpauffley@gmail.com>
;; Maintainer: Oliver Pauffley <mrpauffley@gmail.com>
;; Created: October 28, 2025
;; Modified: October 28, 2025
;; Version: 0.0.1
;; Keywords: emulations games
;; Homepage: https://github.com/oliverpauffley/vaarn.el
;; Package-Requires: ((emacs "25.1"))
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

(defgroup vaarn nil
  "Tools for vaarn referees."
  :group 'tools
  :prefix "vaarn-")

(defvar vaarn--buffer-name "*vaarn-weather*"
  "Buffer name for drawing the weather map to.")

(defcustom vaarn-weather-symbols
  '((still . "s") (dust-storm . "d") (sand-storm . "t") (prismatic-tempest . "P")
    (worm-pollen . "w") (heatwave . "h") (hazy . "a") (rain . "r"))
  "A assoc list of weather and the symbol to be displayed for that weather."
  :type '(alist :key-type string :value-type string)
  :group 'vaarn)

(defcustom vaarn-weather-locations
  '(rain
    sand-storm dust-storm
    dust-storm hazy dust-storm
    dust-storm hazy still heatwave
    worm-pollen hazy dust-storm
    worm-pollen worm-pollen still heatwave
    still still sand-storm
    still dust-storm hazy heatwave
    dust-storm still hazy
    still still still heatwave
    sand-storm sand-storm still
    sand-storm sand-storm
    prismatic-tempest)
  "A list of the weather for the hex grid.
Starting with the top center then working down row by row."
  :type '(list symbol)
  :group 'vaarn)



(defvar vaarn--starting-coord (make-coord
                               :q 0
                               :r 0
                               :s 0)
  "The start space on the weather grid (the centre).")

(defun vaarn--convert-location-to-symbol (locations symbol-list)
  "Convert the LOCATIONS using the SYMBOL-LIST of weather names to symbols."
  (mapcar (lambda (s) (alist-get s symbol-list)) locations))

(defvar vaarn--weather-hex-map
  "
                               X
                             _____
                            /     \\
                       X   /       \\   X
                    ._____(    %s    )_____.
                   /       \\       /       \\
             _____/    %s    \\_____/    %s    \\_____
            /     \\         /     \\         /     \\
      X    /       \\       /       \\       /       \\   X
    ._____(    %s    )_____(    %s    )_____(    %s    )_____.
   /       \\       /       \\       /       \\       /       \\
  /    %s    \\_____/    %s    \\_____/    %s    \\_____/    %s    \\
  \\         /     \\         /     \\         /     \\         /
   \\       /       \\       /       \\       /       \\       /
    )_____(    %s    )_____(    %s    )_____(    %s    )_____(
   /       \\       /       \\       /       \\       /       \\
  /    %s    \\_____/    %s    \\_____/    %s    \\_____/    %s    \\
  \\         /     \\         /     \\         /     \\         /
   \\       /       \\       /       \\       /       \\       /
    )_____(    %s    )_____(    %s    )_____(    %s    )_____(
   /       \\       /       \\       /       \\       /       \\
  /    %s    \\_____/    %s    \\_____/    %s    \\_____/    %s    \\
  \\         /     \\         /     \\         /     \\         /
   \\       /       \\       /       \\       /       \\       /
    )_____(    %s    )_____(    %s    )_____(    %s    )_____(
   /       \\       /       \\       /       \\       /       \\
  /    %s    \\_____/    %s    \\_____/    %s    \\_____/    %s    \\
  \\         /     \\         /     \\         /     \\         /
   \\       /       \\       /       \\       /       \\       /
    \\_____(    %s    )_____(    %s    )_____(    %s    )_____/
       X   \\       /       \\       /       \\       /   X
            \\_____/    %s    \\_____/    %s    \\_____/
                  \\         /     \\         /
                   \\       /       \\       /
                    \\_____(    %s    )_____/
                       X   \\       /   X
                            \\_____/
                               X
"
  "A blank weather hex flower.
With format strings to fill in for the weather in each position.")

(defun vaarn-weather-hex ()
  "Draws a weather hex map in it's own buffer."
  (interactive)
  (let ((buf (get-buffer-create vaarn--buffer-name))
        (symbols (vaarn--convert-location-to-symbol vaarn-weather-locations vaarn-weather-symbols)))
    (with-current-buffer buf
      (erase-buffer)
      (goto-char 0)
      (display-buffer buf)
      (insert (apply 'format vaarn--weather-hex-map symbols)))))


(provide 'vaarn)
;;; vaarn.el ends here
