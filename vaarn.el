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
(load-file "./cube.el")

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

(defvar vaarn--weather-descriptions
  '((still . "Still:\nThe desert landscape is still, untroubled by the susurration of the heavens. Visibility is good.")
    (hazy . "Hazy:\nThe air is still, but mists of a lurid hue hang over the desert. Visibility is impaired and landmarks cannot be seen from a distance. Vigilance checks are made with disadvantage.")
    (dust-storm . "Dust Storm:\nThe wind blows sheets of blue dust across the desert. Visibility is badly impaired. Traveling under such conditions is possible, but the pace is slowed to half normal speed. A three day journey will take six days, and so on. Vigilance checks are made with disadvantage.")
    (sand-storm . "Sand Storm:\nA howling wind blows a ferocious cloud of azure sand across the desert. Nobody travels in Vaarn’s sandstorms; the PCs must hunker down and wait out the storm. Tents or other makeshift shelters will provide adequate protection. Any encounters rolled during these days are assumed to be seeking shelter from the storm in the same place as the party.")
    (heatwave . "Heatwave:\nUrth’s dying sun musters all the warmth it can. PCs must consume twice their normal ration of water per day if they wish to travel during a heatwave.")
    (worm-pollen . "Worm Pollen:\nThe sandworms of the Interior reproduce through a baroque, decade-long process of parthenogenesis, culminating in the explosive release of thousands of melon-sized spores into the atmosphere. This worm-pollen drifts back to Urth in ponderous sticky deluges that can last for weeks. Progress through shifting mounds of the stuff is slowed to half normal speed; the upside is that worm-pollen is edible, and many a starving man has been saved by the timely arrival of spores from the heavens. Treat worm-pollen days as providing d4 rations per player.")
    (rain . "Rain:\nA rare bounty. The parched blue earth is blessed with water. The party may collect 2d6 days of rations per member. In the aftermath of a rainshower, the desert is conquered by a short-lived imperium of majestic flora.")
    (prismatic-tempest ."Prismatic Tempest:\nThe sky bruises with clouds of midnight blue. Howling winds carry scouring sheets of sand across the landscape. Thunder rends the air and polychromatic lightning caresses the desert like the tendrils of a jellyfish deity. No travel of any kind is possible, and the PCs will take 3d6 electrical damage every hour they spend above-ground."))
  "An assoc list of weather and the full description.")

(defcustom vaarn-weather-locations
  `((,(cube-co 0 -3 3) . rain)
    (,(cube-co -1 -2 3) . sand-storm)
    (,(cube-co 1 -3 2) . dust-storm)
    (,(cube-co -2 -1 3) . dust-storm)
    (,(cube-co 0 -2 2) . hazy)
    (,(cube-co 2 -3 1) . dust-storm)
    (,(cube-co -3 0 3) . dust-storm)
    (,(cube-co -1 -1 2) . hazy)
    (,(cube-co 1 -2 1) . still)
    (,(cube-co 3 -3 0) . heatwave)
    (,(cube-co -2 0 2) . worm-pollen)
    (,(cube-co 0 -1 1) . hazy)
    (,(cube-co 2 -2 0) . dust-storm)
    (,(cube-co -3 1 2) . worm-pollen)
    (,(cube-co -1 0 1) . worm-pollen)
    (,(cube-co 1 -1 0) . still)
    (,(cube-co 3 -2 1) . heatwave)
    (,(cube-co -2 1 1) . still)
    (,(cube-co 0 0 0) . still)
    (,(cube-co 2 -1 -1) . sand-storm)
    (,(cube-co -3 2 1) . still)
    (,(cube-co -1 1 0) . dust-storm)
    (,(cube-co 1 0 -1) . hazy)
    (,(cube-co 3 -1 2) . heatwave)
    (,(cube-co -2 2 0) . dust-storm)
    (,(cube-co 0 1 -2) . still)
    (,(cube-co 2 0 -2) . hazy)
    (,(cube-co -3 3 0) . still)
    (,(cube-co -1 2 -1) . still)
    (,(cube-co 1 1 -2) . still)
    (,(cube-co 3 0 -3) . heatwave)
    (,(cube-co -2 3 -1) . sand-storm)
    (,(cube-co 0 2 -2) . sand-storm)
    (,(cube-co 2 1 -3) . still)
    (,(cube-co -1 3 -2) . sand-storm)
    (,(cube-co 1 2 -3) . sand-storm)
    (,(cube-co 0 3 -3) . prismatic-tempest))
  "A list of the weather for the hex grid.
Starting with the top center then working down row by row."
  :type '(alist :key-type sexp :value-type symbol)
  :group 'vaarn)

(defface vaarn--active-location
  '((t :inherit 'bold :foreground "green"))
  "Face for the currently active location on the weather hex map."
  :group 'vaarn)

(defvar vaarn--starting-weather-coord (make-cube-coord
                                       :q 0
                                       :r 0
                                       :s 0)
  "The start space on the weather grid (the centre).")


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

;; TODO mouse over the hexes for their weather?
(defun vaarn-weather-hex ()
  "Draws a weather hex map in it's own buffer."
  (interactive)
  (let* ((buf (get-buffer-create vaarn--buffer-name))
         (current-location vaarn--starting-weather-coord)
         (symbols (vaarn--prepare-display-locations vaarn-weather-locations current-location vaarn-weather-symbols))
         (weather-description (alist-get (alist-get current-location vaarn-weather-locations nil nil #'cube-eq) vaarn--weather-descriptions)))
    (with-current-buffer buf
      (erase-buffer)
      (goto-char 0)
      (display-buffer buf)
      (insert (apply 'format vaarn--weather-hex-map symbols))
      (newline)
      (insert (propertize weather-description 'face 'vaarn--active-location)))))

(defun vaarn--prepare-display-locations (locations current-location symbol-mapping)
  "Convert location weather values LOCATIONS into symbols using SYMBOL-MAPPING.
If the location is the CURRENT-LOCATION then format with a different face."
  (mapcar (lambda (s)
            (let* ((sym (cdr s))
                   (pos (car s))
                   (symbol (alist-get sym symbol-mapping)))
              (if (cube-eq pos current-location)
                  (progn
                    (message "%s" (car s))
                    (propertize symbol 'face 'vaarn--active-location))
                symbol)))
          locations))




(defun vaarn-move-weather-hex (current-coord)
  "Move the from the CURRENT-COORD to a new one.
This means rolling a dice and moving to the corresponding hex.
At edges we carry around the map and back on.
Except at edges marked with `X' where we stay put.
Directions
    2
 1 ___ 3
  /   \
6 \___/ 4
    5"
  (let* ((next-coord (vaarn--next-coord current-coord)))
    ;; do a bounds check on the next-coord
    ))

(defun vaarn--next-coord (c)
  "Calculate a possible next coord from coord C.
This doesn't do any bounds checking it just calculates
the next coord."
  (let ((roll (vaarn--d6)))
    (cond ((= roll 1) (cube-coord-move-nw c))
          ((= roll 2) (cube-coord-move-n))
          ((= roll 3)  (cube-coord-move-ne c))
          ((= roll 4)  (cube-coord-move-se c))
          ((= roll 5)  (cube-coord-move-s c))
          (t  (cube-coord-move-sw c)))
    c))

(defun vaarn--d6 (cube-co )
  "Roll a 6 sided dice."
  (vaarn--roll-dice 6))

(defun vaarn--roll-dice (size)
  "Roll a SIZE sided dice."
  (+ (random size) 1))

(provide 'vaarn)
;;; vaarn.el ends here
