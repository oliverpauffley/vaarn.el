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


(defgroup vaarn nil
  "Tools for vaarn referees."
  :group 'tools
  :prefix "vaarn-")

(defconst vaarn--weather-buffer-name "*vaarn-weather*"
  "Buffer name for drawing the weather map to.")

(defcustom vaarn-weather-symbols
  '((still . "s") (duststorm . "d") (sandstorm . "t") (prismatictempest . "P")
    (wormpollen . "w") (heatwave . "h") (hazy . "a") (rain . "r"))
  "A assoc list of weather and the symbol to be displayed for that weather."
  :type '(alist :key-type symbol :value-type string)
  :group 'vaarn)

(defvar vaarn--weather-descriptions
  '((still . "Still:\nThe desert landscape is still, untroubled by the susurration of the heavens. Visibility is good.")
    (hazy . "Hazy:\nThe air is still, but mists of a lurid hue hang over the desert. Visibility is impaired and landmarks cannot be seen from a distance. Vigilance checks are made with disadvantage.")
    (duststorm . "Dust Storm:\nThe wind blows sheets of blue dust across the desert. Visibility is badly impaired. Traveling under such conditions is possible, but the pace is slowed to half normal speed. A three day journey will take six days, and so on. Vigilance checks are made with disadvantage.")
    (sandstorm . "Sand Storm:\nA howling wind blows a ferocious cloud of azure sand across the desert. Nobody travels in Vaarn’s sandstorms; the PCs must hunker down and wait out the storm. Tents or other makeshift shelters will provide adequate protection. Any encounters rolled during these days are assumed to be seeking shelter from the storm in the same place as the party.")
    (heatwave . "Heatwave:\nUrth’s dying sun musters all the warmth it can. PCs must consume twice their normal ration of water per day if they wish to travel during a heatwave.")
    (wormpollen . "Worm Pollen:\nThe sandworms of the Interior reproduce through a baroque, decade-long process of parthenogenesis, culminating in the explosive release of thousands of melon-sized spores into the atmosphere. This wormpollen drifts back to Urth in ponderous sticky deluges that can last for weeks. Progress through shifting mounds of the stuff is slowed to half normal speed; the upside is that wormpollen is edible, and many a starving man has been saved by the timely arrival of spores from the heavens. Treat wormpollen days as providing d4 rations per player.")
    (rain . "Rain:\nA rare bounty. The parched blue earth is blessed with water. The party may collect 2d6 days of rations per member. In the aftermath of a rainshower, the desert is conquered by a short-lived imperium of majestic flora.")
    (prismatictempest . "Prismatic Tempest:\nThe sky bruises with clouds of midnight blue. Howling winds carry scouring sheets of sand across the landscape. Thunder rends the air and polychromatic lightning caresses the desert like the tendrils of a jellyfish deity. No travel of any kind is possible, and the PCs will take 3d6 electrical damage every hour they spend above-ground."))
  "An assoc list of weather and the full description.")

(defcustom vaarn-weather-locations
  `((,(cube-co 0 -3 3) . rain)
    (,(cube-co -1 -2 3) . sandstorm)
    (,(cube-co 1 -3 2) . duststorm)
    (,(cube-co -2 -1 3) . duststorm)
    (,(cube-co 0 -2 2) . hazy)
    (,(cube-co 2 -3 1) . duststorm)
    (,(cube-co -3 0 3) . duststorm)
    (,(cube-co -1 -1 2) . hazy)
    (,(cube-co 1 -2 1) . still)
    (,(cube-co 3 -3 0) . heatwave)
    (,(cube-co -2 0 2) . wormpollen)
    (,(cube-co 0 -1 1) . hazy)
    (,(cube-co 2 -2 0) . duststorm)
    (,(cube-co -3 1 2) . wormpollen)
    (,(cube-co -1 0 1) . wormpollen)
    (,(cube-co 1 -1 0) . still)
    (,(cube-co 3 -2 -1) . heatwave)
    (,(cube-co -2 1 1) . still)
    (,(cube-co 0 0 0) . still)
    (,(cube-co 2 -1 -1) . sandstorm)
    (,(cube-co -3 2 1) . still)
    (,(cube-co -1 1 0) . duststorm)
    (,(cube-co 1 0 -1) . hazy)
    (,(cube-co 3 -1 -2) . heatwave)
    (,(cube-co -2 2 0) . duststorm)
    (,(cube-co 0 1 -1) . still)
    (,(cube-co 2 0 -2) . hazy)
    (,(cube-co -3 3 0) . still)
    (,(cube-co -1 2 -1) . still)
    (,(cube-co 1 1 -2) . still)
    (,(cube-co 3 0 -3) . heatwave)
    (,(cube-co -2 3 -1) . sandstorm)
    (,(cube-co 0 2 -2) . sandstorm)
    (,(cube-co 2 1 -3) . still)
    (,(cube-co -1 3 -2) . sandstorm)
    (,(cube-co 1 2 -3) . sandstorm)
    (,(cube-co 0 3 -3) . prismatictempest))
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

(define-multisession-variable vaarn--current-weather-coord nil
  "The current space on the weather grid."
  :package "vaarn"
  :synchronized t
  :storage 'sqlite)

(defun vaarn-reset-weather ()
  "Reset the weather to the centre."
  (interactive)
  (setf (multisession-value vaarn--current-weather-coord) vaarn--starting-weather-coord)
  (vaarn-weather-hex))

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


(defun vaarn--weather-get-current-weather (location)
  "Return the current weather at LOCATION."
  (alist-get location vaarn-weather-locations nil nil #'equal))

(defun vaarn--weather-get-description (name)
  "Given a NAME symbol return the description."
  (alist-get name vaarn--weather-descriptions))

(defun vaarn--draw-weather-hex (location)
  "Draws a weather hex map in it's own buffer, with LOCATION marked."
  (let* ((buf (get-buffer-create vaarn--weather-buffer-name))
         (symbols (vaarn--weather-prepare-display-locations vaarn-weather-locations location vaarn-weather-symbols))
         (weather-description (vaarn--weather-get-description (vaarn--weather-get-current-weather location))))
    (save-excursion
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (erase-buffer)
        (vaarn-weather-mode 1)
        (goto-char 0)
        (insert (apply 'format vaarn--weather-hex-map symbols))
        (insert (propertize weather-description 'face 'vaarn--active-location))
        (goto-char 0)
        (setq buffer-read-only t)))))

(defun vaarn--weather-prepare-display-locations (locations current-location symbol-mapping)
  "Convert location weather values LOCATIONS into symbols using SYMBOL-MAPPING.
If the location is the CURRENT-LOCATION then format with a different face.
Also adds the weather descriptions as a mouse hover tooltip"
  (mapcar (lambda (s)
            (let* ((sym (cdr s))
                   (pos (car s))
                   (symbol (alist-get sym symbol-mapping))
                   (hover-symbol (propertize symbol 'help-echo (symbol-name sym))))
              (if (equal pos current-location)
                  (propertize hover-symbol 'face 'vaarn--active-location)
                hover-symbol)))
          locations))


(defun vaarn-move-weather-hex ()
  "Move the from the current coord to a new one.
This means rolling a dice and moving to the corresponding hex.
At edges we carry around the map and back on.
Except at edges marked with `X' where we stay put.
Directions
    2
 1 ___ 3
  /   \
6 \___/ 4
    5"
  (interactive)
  (let* ((current-coord (or (multisession-value vaarn--current-weather-coord) vaarn--starting-weather-coord))
         (next-coord (vaarn--next-coord current-coord)))
    (setf (multisession-value vaarn--current-weather-coord) next-coord)
    (if (not (vaarn--visible-buffer-p vaarn--weather-buffer-name))
        (message "Weather is %s" (vaarn--weather-get-description (vaarn--weather-get-current-weather next-coord))))
    (vaarn--draw-weather-hex next-coord)))

(defun vaarn--visible-buffer-p (buf)
  "Return non-nil if BUF is visible."
  (get-buffer-window buf))


;;;###autoload
(defun vaarn-weather-hex ()
  "Draws a weather hex map in it's own buffer.
Either loads the location or sets to a default value."
  (interactive)
  (let
      ((buf (get-buffer-create vaarn--weather-buffer-name)))
    (display-buffer buf)
    (let ((coord (if (cube-coord-p (multisession-value vaarn--current-weather-coord))
                     (multisession-value vaarn--current-weather-coord)
                   vaarn--starting-weather-coord)))
      (vaarn--draw-weather-hex coord))))


(defun vaarn--next-coord (c)
  "Calculate a possible next coord from coord C.
If the new coord is out of bounds this might wrap or return the same coord as C."
  (let* ((roll (vaarn--d6))
         (maybe-new-coord (cond ((= roll 1) (cube-coord-move-nw c))
                                ((= roll 2) (cube-coord-move-n c))
                                ((= roll 3)  (cube-coord-move-ne c))
                                ((= roll 4)  (cube-coord-move-se c))
                                ((= roll 5)  (cube-coord-move-s c))
                                ((= roll 6)  (cube-coord-move-sw c))))

         (new-coord (cond
                     ;; if we are still in the grid then we can just return this value
                     ((not (vaarn--weather-out-of-bounds-p maybe-new-coord)) maybe-new-coord)
                     ;; if the next coord is one of the `X's then stay put
                     ((vaarn--weather-in-x-coord-p maybe-new-coord) c)
                     ;; we must be off grid but not in x so loop around
                     (t (vaarn--weather-loop-coord roll c)))))
    new-coord))

(defun vaarn--weather-out-of-bounds-p (c)
  "Return non nil if the coordinates C is off the map."
  (or (> (abs (cube-coord-q c)) 3)
      (> (abs (cube-coord-r c)) 3)
      (> (abs (cube-coord-s c)) 3)))

(defvar vaarn--weather-x-coords
  `(,(cube-co 0 -4 4)
    ,(cube-co -1 -3 4)
    ,(cube-co 1 -4 3)
    ,(cube-co -3 -1 4)
    ,(cube-co 3 -4 1)
    ,(cube-co 0 4 -4)
    ,(cube-co -1 4 -3)
    ,(cube-co 1 3 -4)
    ,(cube-co -3 4 -1)
    ,(cube-co 3 1 -4)))

(defun vaarn--weather-in-x-coord-p (c)
  "Return non nil if the coordinate C is in one of the `X's."
  (seq-find (lambda (val) (equal c val)) vaarn--weather-x-coords))

(defun vaarn--weather-loop-coord (roll c)
  "Gived a ROLL and C coordinate, loop around the grid round to the otherside.
This is actually just swapping the two coordinates in the stepped direction."
  (cond ((or (= roll 2)(= roll 5)) (cube-coord-flip-not-q c))
        ((or (= roll 3)(= roll 6)) (cube-coord-flip-not-s c))
        ((or (= roll 4)(= roll 1))  (cube-coord-flip-not-r c))))

(defun vaarn--d6 ()
  "Roll a 6 sided dice."
  (vaarn--roll-dice 6))

(defun vaarn--roll-dice (size)
  "Roll a SIZE sided dice."
  (+ (random size) 1))

;;;###autoload
(define-minor-mode vaarn-weather-mode
  "Move around the vaarn weather hex map to generate weather.
\\<vaarn-weather-mode-map>
Use \\[vaarn-weather-hex] to start a new map.
Then use \\[vaarn-move-weather-hex] to move around the map.

\\{vaarn-weater-mode-map}"
  :lighter " Vaarn Weather"
  :keymap `((, (kbd "r") . vaarn-move-weather-hex)
            (, (kbd "e") . vaarn-reset-weather)))


(provide 'vaarn)
;;; vaarn.el ends here
