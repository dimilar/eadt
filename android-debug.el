;; File            : android-debug.el
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
;; This file contains a VC backend for the git version control
;; system.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; android-debug.el --- 
;; Created: Mi Sep 14 05:07:07 2011 (+0200)
;; Author: Shulei Zhu
;; 
;;; Code:

(require 'android)

(defcustom android-sdk-jdb-port "29882"
  "default port supplied to `android-activity-jdb-debug'"
  :type 'string
  :group 'android-mode)

(defcustom android-jni-gdb-port "5039"
  "Default port supplied to `android-jni-gdb-port'."
  :type 'string
  :group 'android-mode)

(defcustom android-native-gdb-port "1234"
  "Default port supplied to `android-native-gdb-debug'."
  :type 'string
  :group 'android-mode)

(defcustom android-gdbserver-path "/data/tmp"
  "For `android-native-gdb-debug', the default place to store
the gdbserver which aims at the arm platform."
  :type 'string
  :group 'android-mode)

(defcustom android-native-program-dir "/data/tmp"
  "The default place to put the binary program for `android-native-gdb-debug'"
  :type 'string
  :group 'android-mode)

(defun android-package-class ()
  "History of pakcakges(name) and pathes of android projects. Return a list consisting of
two elements, which contain classes and pathes respectively. Note the CAR the each element is usually
the class of path of the current open project."
  (let ((current-project (intern android-file-name android-file-prop-obarray))
        src-path source-dir package class class-list path-list project)
    (dotimes (iter 10)
      (unless (eq (setq project (aref android-project-prop-obarray iter)) 0)
        (when (and (setq package (get project 'package))
                 (setq class (get project 'android:name)))
          (if (setq source-dir (get project 'source.dir))
              (setq src-path source-dir)
            (setq src-path
                  (concat "src/" (mapconcat #'concat (split-string package "\\.") "/") "/")))
          (if (string= (symbol-name project) (get current-project 'project-root))
              (progn
                (push (cons package  class) class-list)
                (push (cons (symbol-name project) src-path) path-list))
            (progn
              (setq class-list (append class-list (list (cons package class))))
              (setq path-list (append path-list (list  (cons (symbol-name project) src-path))))
              )))))
    (list class-list path-list)))

(defun android-get-process-id (device package-name)
  "Execute the command >adb -s your-device shell ps< and dump all results into a
buffer, then parse the buffer to exract the process id of the given package. Return the
process id or nil."  
  (let ((adb-tool-path (android-get-tool-path "adb"))
        (buffer (concat "*android-get-process-id-" package-name))
        id)
    (call-process-shell-command
     adb-tool-path nil buffer nil (format " %s shell ps" device))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward
       (concat "\\([0-9]+\\)\\( +?[0-9]+?\\)\\\{3\\\}\\( +?[0-9a-zA-Z]+?\\)\\\{2\\\} [A-Z] \\(.*\\)?"
               (regexp-quote package-name) "$") nil t)
      (setq id (match-string 1))
      (kill-buffer buffer)) id))

(defun adb-forward-port  (device protocol &optional port-pair)
  "Forwarding of requests on a specific host port to a different
port on an emulator/device instance. If optional parameter port-pair
 is nil, we have to find the jdwp port by ourself. If success, return 0,
otherwise return a positive number (error code)."
  (let* (local-port
         remote-port
         (ports
          (if (consp port-pair)
              port-pair
            (let ((shell-output
                   (shell-command-to-string
                    (format "adb %s jdwp | tail -1" device))))
              (if (string-match  "\\([0-9]+\\)" shell-output)
                  (cons android-sdk-jdb-port (match-string 1 shell-output))
                nil)))))
    (when ports
      (setq local-port (car ports))
      (setq remote-port (cdr ports))
      (call-process-shell-command
       (android-get-tool-path "adb") nil nil nil
       (format " forward tcp:%s %s:%s" local-port protocol remote-port)))))

(defun android-activity-jdb-debug ()
  " Function to debug the activity using jdb. steps:
1. forward a remote port to a local port
2. interactively run the `jdb' command
3. sleep a second to waiting for the `jdb' finished
4. launch the activity from the device or emulator
Please kill the existent process of the package to be debugged."
  (interactive)
  (let* ((device (android-get-current-device))
         (device-arg (if device (format " -s %s " device) ""))
         (adb-tool-path (android-get-tool-path "adb"))
         (package-info (android-package-class))
         (classes (mapcar #'(lambda (x) (concat (car x) "/." (cdr x))) (nth 0 package-info)))
         (pathes (mapcar #'(lambda (x) (concat (car x) "src")) (nth 1 package-info)))
         (class (completing-read "Package: " classes nil nil (car classes) nil (car classes)))
         (package-class (split-string class "/\\."))
         (source-path (completing-read "Source path: " pathes nil nil (car pathes) nil (car pathes)))
         (jdb-command
          (concat "jdb -connect com.sun.jdi.SocketAttach:hostname=localhost,port="
                  android-sdk-jdb-port " -sourcepath" source-path))
         package-process-id)
    ;; kill the existent process
    ;; (when (setq package-process-id (android-get-process-id device-arg (car package-class)))
    ;;   (call-process-shell-command
    ;;    adb-tool-path nil nil nil
    ;;    (format " %s shell kill -9 %s" device-arg package-process-id))
    ;;   (call-process-shell-command "sleep" nil nil nil " 5"))    
    (if (eq (adb-forward-port device-arg "jdwp") 0)
        (progn 
        ;; (call-process-shell-command adb-tool-path nil nil nil adb-start-args)
          ;; (android-launch-activity device-arg package-class)
          (jdb jdb-command)
          (call-process-shell-command "sleep" nil nil nil " 1")
          (start-process-shell-command
           "*android start activity*" nil
           (format "%s %s shell am start -n %s" adb-tool-path device-arg (car classes))))
      (error "Failed to forward the port"))))

(defun android-jni-gdb-debug ()
  "Function to debug the jni library. Please refer the script ndk-debug."
  (interactive)
  (let* ((adb-tool-path (android-get-tool-path "adb"))
         (package-info (android-package-class))
         (projects-root (mapcar #'(lambda (x) (car x))
                                (nth 1 package-info)))
         (classes (mapcar #'(lambda (x) (concat (car x) "/." (cdr x)))
                          (nth 0 package-info)))
         (class
          (completing-read "Package: "
                           classes nil nil
                           (car classes) nil (car classes)))
         (proj-root
          (completing-read "Project root: "
                           projects-root nil nil
                           (car projects-root) nil (car projects-root)))
         (package-class (split-string class "/\\."))
         (package-name (car package-class))
         (device (android-get-current-device))
         (device-arg (if device (format " -s %s " device) ""))
         (build-local-mk
          (concat (nth 1 (assoc "ANDROID_NDK" android-environment-variables))
                  "/build/core/build-local.mk"))
         package-process-id gdbserver-process-id abi
         data-dir app-out app-process debug-socket)
    ;; (setq package-process-id (android-launch-activity device-arg package-class))
    (start-process-shell-command
     "*android start activity*" nil
     (format "%s %s shell am start -n %s" adb-tool-path device-arg (car classes)))
    (setq gdbserver-process-id (android-get-process-id device-arg "gdbserver"))
    (when gdbserver-process-id
      (call-process-shell-command
       adb-tool-path nil nil nil
       (format " % shell kill -9 %s" device-arg gdbserver-process-id)))
    (setq data-dir
          (substring
           (shell-command-to-string
            (format "%s %s shell run-as %s /system/bin/sh -c pwd"
                    adb-tool-path device-arg package-name)) 0 -1))
    (setq debug-socket (concat "localfilesystem:" data-dir "/debug-socket"))
    (setq abi (android-get-abi))
    (setq app-out (substring
                   (shell-command-to-string
                     (format "make --no-print-dir -f %s -C\
 %s DUMP_TARGET_OUT APP_ABI=%s"
                             build-local-mk proj-root (car abi))) 0 -1))
    (setq default-directory app-out)
    (call-process-shell-command
     adb-tool-path nil nil nil
     (format " %s shell pull /system/bin/app_process %s/app_process" device-arg app-out))
    (start-process-shell-command "*android-start-remote-gdbserver*" nil
                                 (format "%s %s shell run-as %s lib/gdbserver +debug-socket --attach %s"
                                         adb-tool-path device-arg  package-name package-process-id))
    ;; (port-pair (cons android-native-gdb-port android-native-gdb-port))
    ;; (call-process-shell-command "sleep" nil nil nil " 2")
    (call-process-shell-command adb-tool-path nil nil nil
                                (format " %s forward tcp:%s %s"
                                        device-arg android-jni-gdb-port debug-socket))
    ;; (gdb (format " -x %s  -e  %s/app_process" (concat app-out "/gdb.setup") app-out))
    (call-interactively 'gdb)
    ;; (error (format "package: %s is not running" package-name)
    ))

(defun android-native-gdb-debug ()
  "Function to debug the native program. Steps:
1. if there is no gdbserver available on your device or emulator, copy one
2. if any existent debugging process is running, kill them (both program and gdbserver)
3. forward port
4. run gdb interactively"
  (interactive)
  (let* ((app (get (intern android-file-name android-file-prop-obarray) 'program))
         (device (android-get-current-device))
         (device-arg (if device (format " -s %s " device) ""))
         (program
          (completing-read "Program: " nil nil nil
                           app
                           nil app))
         (adb-tool-path (android-get-tool-path "adb"))
         (program-full-path (concat android-native-program-dir "/" program))
         (gdbserver-full-path (concat android-gdbserver-path "/gdbserver"))
         (gdbserver-exist (string= (substring
                                    (shell-command-to-string
                                     (format "%s %s shell ls %s"
                                             (android-get-tool-path "adb")
                                             device-arg gdbserver-full-path)) 0 -1)
                                   gdbserver-full-path))
         (program-pid (android-get-process-id device-arg program-full-path))
         (gdbserver-pid (android-get-process-id device-arg gdbserver-full-path))
         (command (format "%s %s shell %s :%s %s"
                          adb-tool-path
                          device-arg
                          gdbserver-full-path
                          android-native-gdb-port
                          program-full-path))
        (port-pair (cons android-native-gdb-port android-native-gdb-port)))
    (unless gdbserver-exist
      (call-process-shell-command
       (android-get-tool-path "adb") nil nil nil
       (format " %s push %s %s"
               device-arg (android-get-tool-path "gdbserver") gdbserver-full-path)))
    (when gdbserver-pid
      (call-process-shell-command
       adb-tool-path nil nil nil
       (format " %s shell kill -9 %s" device-arg gdbserver-pid)))
    (when program-pid
      (call-process-shell-command
       adb-tool-path nil nil nil
       (format " %s shell kill -9 %s" device-arg program-pid)))
    (adb-forward-port device-arg "tcp" port-pair)
    (start-process-shell-command
     (format "*android debug: %s*" program-full-path) nil command)
    (call-interactively 'gdb)))
(provide 'android-debug)
