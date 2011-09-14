;; File            : android-target.el
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
;;; android-target.el --- 
;; Created: Mi Sep 14 05:08:23 2011 (+0200)
;; Author: Shulei Zhu
;; 
;;; Code:

(defvar android-targets-all-alist '())
(defvar android-targets-available-alist '())
(defvar android-targets-list '())
;; (setq android-targets-available-alist nil)
;; (setq android-targets-all-alist nil)
(defun android-get-targets-list()
  (let (target-string)
    (unless android-targets-all-alist (android-list-targets))
    (when android-targets-all-alist
      (dolist (target android-targets-all-alist)
        (setq target-string (concat (get target 'id) ": " (get target 'alias) " <" (get target 'name) ">"))
        (unless (member target-string android-targets-list)
          (dolist (available-target android-targets-available-alist)
            (when (and (string= (get target 'api-level) (get available-target 'api-level))
                       (string= (get target 'name) (get available-target 'target)))
              (push target-string android-targets-list)))
          (setq android-targets-list (append android-targets-list (list target-string))))))
    android-targets-list))

(defun android-list-targets ()
  (let ((buffer "*android list targets*")
       target available-targets (available-count 0))
    (call-process-shell-command
     (android-get-tool-path "android")
     nil buffer nil "list")
    (save-excursion
      (save-window-excursion
        (with-current-buffer buffer
          (goto-char (point-min))
          (while (re-search-forward
                  "id: \\([0-9]+?\\) or \\\"\\(.*?\\)\\\"\n +?Name:\
 \\(.*?\\)\n +?Type: \\(.*?\\)\n\\( +?Vendor: .*?\n +?Revision: .*?\n\
 +?Description: .*\n +?Based on .* \\| +?API level: \\)\\([0-9]\\{1,2\\}\\).*" nil t)
            (setq target (intern (concat "target-" (match-string 1))))
            (unless (memq target android-targets-all-alist)
              (push target android-targets-all-alist)
              (setplist target nil)
              (put target 'id (match-string 1))
              (put target 'alias (match-string 2))
              (put target 'name (match-string 3))
              (put target 'type (match-string 4))
              (put target 'api-level (match-string 6))))
          (while (re-search-forward
                  "Available Android Virtual Devices:\n +?Name: \\(.*?\\)\n.*\n +?\
Target: \\(.*\\) (API level \\([0-9]+?\\))" nil t)
            (setq target (intern (concat "avd-" (number-to-string (1+ available-count)))))
            (unless (memq target android-targets-available-alist)
              (push target android-targets-available-alist)
              (put target 'name (match-string 1))
              (put target 'target (match-string 2))
              (put target 'api-level (match-string 3))))
          (kill-buffer buffer))))))

(defun android-targets-reload ()
  ""
  (interactive)
  (setq android-targets-all-alist nil
        android-targets-available-alist nil
        android-targets-list nil)
  (android-list-targets))

(provide 'android-target)
