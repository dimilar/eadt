;;; android-launch.el --- 

;; Copyright (C) 2009, 2010, 2011 R.W van 't Veer

;; Author: R.W. van 't Veer
;; Modified by: S.L Zhu <schuleichu@gmail.com>
;; Created: 20 Feb 2009
;; Keywords: tools processes
;; Version: 0.1
;; URL: https://github.com/remvee/android-mode

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'android)
(defvar android-mode-log-face-alist
  '(("V" . android-mode-verbose-face)
    ("D" . android-mode-debug-face)
    ("I" . android-mode-info-face)
    ("W" . android-mode-warning-face)
    ("E" . android-mode-error-face)))

(defvar android-exclusive-processes ())

(defface android-mode-verbose-face '((t (:foreground "DodgerBlue")))
  "Font Lock face used to highlight VERBOSE log records."
  :group 'android-mode)

(defface android-mode-debug-face '((t (:foreground "ForestGreen")))
  "Font Lock face used to highlight DEBUG log records."
  :group 'android-mode)

(defface android-mode-info-face '((t (:foreground "Gray45")))
  "Font Lock face used to highlight INFO log records."
  :group 'android-mode)

(defface android-mode-warning-face '((t (:foreground "Red")))
  "Font Lock face used to highlight WARN log records."
  :group 'android-mode)

(defface android-mode-error-face '((t (:foreground "Red" :bold t)))
  "Font Lock face used to highlight ERROR log records."
  :group 'android-mode)

(defcustom android-logcat-buffer "*android-logcat*"
  "Name for the buffer where logcat output goes."
  :type 'string
  :group 'android-mode)

(defun android-start-exclusive-command (name command &rest args)
  (and (not (memq (intern name) android-exclusive-processes))
       (set-process-sentinel (apply 'start-process-shell-command name name command args)
                             (lambda (proc msg)
                               (when (memq (process-status proc) '(exit signal))
                                 (setq android-exclusive-processes
                                       (delete (intern (process-name proc))
                                               android-exclusive-processes)))))
       (setq android-exclusive-processes (cons (intern name)
                                               android-exclusive-processes))))

;;;###autoload
(defun android-logcat-find-file ()
  (interactive)
  (let ((filename (get-text-property (point) 'filename))
        (linenr (get-text-property (point) 'linenr)))
    (when filename
      (find-file (concat (get (intern filename android-file-prop-obarray)
                              'project-root) "/src/" filename))
      ;; (goto-line linenr)
      (goto-char (point-min)) (forward-line (1- linenr)))))

;;;###autoload
(defun android-logcat-find-file-mouse (event)
  (interactive "e")
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
            pos (posn-point (event-end event)))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (android-logcat-find-file))))

(defvar android-logcat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'android-logcat-find-file)
    (define-key map [mouse-2] 'android-logcat-find-file-mouse)
    map))

(defun android-list-avd ()
  "List of Android Virtual Devices installed on local machine."
  (let* ((command (concat (android-get-tool-path "android") " list avd"))
         (output (shell-command-to-string command))
         (result nil)
         (offset 0))
    (while (string-match "Name: \\(.*\\)" output offset)
      (setq result (cons (match-string 1 output) result))
      (setq offset (match-end 0)))
    (if result
        (reverse result)
      (error "no Android Virtual Devices found"))))

(defun android-logcat-prepare-msg (msg)
  (if (string-match "\\bat \\(.+\\)\\.\\([^.]+\\)\\.\\([^.]+\\)(\\(.+\\):\\([0-9]+\\))" msg)
      (let* ((package (match-string 1 msg))
             (class (match-string 2 msg))
             (method (match-string 3 msg))
             (filename (concat (replace-regexp-in-string "\\." "/" package) "/" (match-string 4 msg)))
             (linenr (match-string 5 msg)))
        (if (file-exists-p
             (concat (get (intern filename android-file-prop-obarray)
                          'project-root) "/src/" filename))
            (propertize msg
                        'face 'underline
                        'mouse-face 'highlight
                        'filename filename
                        'linenr (string-to-number linenr)
                        'follow-link t)
          msg))
    msg))

(defvar android-logcat-pending-output "")

(defun android-logcat-process-filter (process output)
  "Process filter for displaying logcat output."
  (with-current-buffer android-logcat-buffer
    (let ((following (= (point-max) (point)))
          (buffer-read-only nil)
          (pos 0)
          (output (concat android-logcat-pending-output
                          (replace-regexp-in-string "" "" output))))
      (save-excursion
        (while (string-match "\n" output pos)
          (let ((line (substring output pos (match-beginning 0))))
            (setq pos (match-end 0))
            (goto-char (point-max))
            (if (string-match "^\\(.\\)/\\(.*\\)( *\\([0-9]+\\)): \\(.*\\)$" line)
                (let* ((level (match-string 1 line))
                       (level-face (or (cdr (assoc level android-mode-log-face-alist)) 'android-mode-info-face))
                       (tag (replace-regexp-in-string " *$" "" (match-string 2 line)))
                       (pid (match-string 3 line))
                       (msg (match-string 4 line)))
                  (insert (propertize level
                                      'font-lock-face level-face))
                  (tab-to-tab-stop)
                  (insert (propertize tag
                                      'font-lock-face 'font-lock-function-name-face))
                  (insert (propertize (concat "("  pid ")")
                                      'font-lock-face 'font-lock-constant-face))
                  (tab-to-tab-stop)
                  (insert (android-logcat-prepare-msg (propertize msg 'font-lock-face level-face))))
              (insert (propertize line
                                  'font-lock-face 'font-lock-warning-face)))
            (insert "\n")))
        (setq android-logcat-pending-output (substring output pos)))
      (when following (goto-char (point-max))))))

;;;###autoload
(defun android-logcat ()
  "Switch to ADB logcat buffer, create it when it doesn't exists yet."
  (interactive)
  (when (android-start-exclusive-command android-logcat-buffer
                                         (android-get-tool-path "adb")
                                         "logcat")
    (set-process-filter (get-buffer-process android-logcat-buffer)
                        #'android-logcat-process-filter)
    (with-current-buffer android-logcat-buffer
      (setq buffer-read-only t)
      (set (make-local-variable 'tab-stop-list) '(2 30))
      (use-local-map android-logcat-map)
      (font-lock-mode t)
      (android-mode t)))
  (switch-to-buffer android-logcat-buffer)
  (goto-char (point-max)))

;;;###autoload
(defun android-launch-emulator ()
  "start emulator"
  (interactive)
  (let* ((all-avd
          (progn (unless android-targets-available-alist (android-list-targets))
                 (if android-targets-available-alist
                     (mapcar #'(lambda (x) (get x 'name)) android-targets-available-alist) nil)))
         (avd (completing-read "AVD: " all-avd nil nil (car all-avd) nil (car all-avd))))
    (unless (android-start-exclusive-command
             (concat "*android-emulator-" avd "*")
             (concat (android-get-tool-path "emulator") " -avd " avd)
             (message (concat "emulator " avd " already running"))))))

;;;###autoload
(defun android-launch-ddms ()
  "Launch Dalvik Debug Monitor Service tool."
  (interactive)
  (unless (android-start-exclusive-command "*android-ddms*" (android-get-tool-path "ddms"))
    (message "ddms already running")))

(provide 'android-launch)

