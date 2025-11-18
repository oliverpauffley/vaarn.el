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
(require 'vaarn-dice)

(defgroup vaarn nil
  "Tools for vaarn referees."
  :group 'tools
  :prefix "vaarn-")


(provide 'vaarn)
;;; vaarn.el ends here
