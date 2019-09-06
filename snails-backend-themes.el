;;; snails-backend-themes.el --- snails and themes

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (snails)
;; Homepage: https://github.com/shanyou/snails-backend
;; Keywords: load themes


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;;
;; Use `snails' to select themes
;;

;;; Installation:

;; Please download `snails' first, Add snails to `load-path'
;; The snails links is https://github.com/manateelazycat/snails
;; (add-to-list 'load-path "/path/to/snails")
;;
;; put snails-backend-themes to your load-path,
;; The load-path is usually ~/elisp/
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.

;; (require 'snails-backend-themes)
;; (push 'snails-backend-themes snails-default-backends)
;; M-x snails-load-themes

;;; package Requires
(require 'snails-core)

;;; Code:

(snails-create-sync-backend
 :name
  "THEMES"

  :candidate-filter
  (lambda (input)
    (let ((candidates))
      (dolist (theme (mapcar  #'symbol-name (custom-available-themes)))
        (when (or
               (string-equal input "")
               (snails-match-input-p input theme))
          (snails-add-candiate 'candidates theme theme)))
      (snails-sort-candidates input candidates 0 0)
      candidates))

  :candiate-do
  (lambda (candidate)
    (disable-theme custom-enabled-themes)
    (load-theme (intern candidate) t)))

(defun snails-load-themes ()
  "Loading a theme use `snails'"
  (interactive)
  (snails '(snails-backend-themes)))

(provide 'snails-backend-themes)

;;; snails-backend-themes.el ends here
