;; File            : android.el
;; 
;; Copyright (C) 2011 Free Software Foundation, Inc.
;; 
;; Author: Shulei Zhu <schuleichu@gmail.com>
;; Keywords: android-mode
;; Package: android
;; 
;; This file is part of GNU Emacs.
;; 
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;; Commentary:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; android.el --- 
;; Created: Mi Sep 14 05:07:24 2011 (+0200)
;; Author: Shulei Zhu
;; 
;;; Code:

(defconst android-version "0.0.1")
(defgroup android-mode nil
  ""
  :prefix "android-mode-"
  :group 'applications)

(defvar android-project-name nil)
(defvar android-native-project-type "c")

(make-variable-buffer-local 'android-project-name)
(make-variable-buffer-local 'android-native-project-type)

(require 'tempo)
(require 'compile)


;; (require 'android-tools)
(require 'android-init)
;; (require 'android-command)
;; (require 'android-debug)
;; (require 'android-launch)
;; (require 'android-target)
;; (require 'android-project)
;; (require 'android-gen)

(defvar android-minor-mode-map (android-minor-keymap))

;;;###autoload
(define-minor-mode android-mode
  "Android application development minor mode."
  :group 'android-mode
  :global nil
  :lighter android-mode-string
  :keymap android-minor-mode-map
  :require 'android)


;; (put 'android-mode 'risky-local-variable t)
;; (make-variable-buffer-local 'android-mode)
;; (put 'android-mode 'permanent-local t)

;;;###autoload
(defun android-find-file-hook ()
  "Function for `find-file-hook' activating android minor mode mode if appropriate."
  (when android-mode
    (android-mode nil))
  (if buffer-file-name
      (setq android-file-name buffer-file-name)
    (setq android-file-name default-directory))
  (when android-file-name
    (android-file-clear-prop android-file-name)
    (when (android-file-set-prop android-file-name)
      (android-switch-build-tool
       (get (intern android-file-name android-file-prop-obarray) 'build-tool))
      (android-switch-device (car (get (intern android-file-name android-file-prop-obarray) 'device)))
      ;; (android-refresh-device-menu)
      (android-mode t))))

;; (add-hook 'dired-mode-hook (lambda () (when (android-root) (android-mode t))))
;;;###autoload
(add-hook 'find-file-hook  'android-find-file-hook)
;;;###autoload
(add-hook 'dired-mode-hook  'android-find-file-hook)
(provide 'android)
