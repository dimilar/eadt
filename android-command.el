;; File            : android-command.el
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
;;; android-command.el --- 
;; Created: Mi Sep 14 05:07:00 2011 (+0200)
;; Author: Shulei Zhu
;; 
;;; Code:


(defun android-ant-command (task &optional args)
  "The interface to run command \"ant compile|install|uninstall\""
  (when android-file-name
    (let* ((file-sym (intern android-file-name android-file-prop-obarray))
           (default-directory (get file-sym 'project-root))
           (device (android-get-current-device))
           (device-arg (when device (format " -Dadb.device.arg=\"-s %s\" " device))))
      (compile (concat (android-get-tool-path "ant") device-arg " " task)))))

;; TODO
(defun android-ndk-command (task &optional args)
  "The interface to run command \"ndk-build\""
  (when android-file-name
    (let ((default-directory (get (intern android-file-name android-file-prop-obarray) 'project-root))
          (local-task (if (string= task "compile") "" task)))
      (compile (concat (android-get-tool-path "ndk-build") " " local-task)))))

(defun android-cmake-command (task &optional args)
  "The interface to run command \"cmake ../ & make install|uninstall|run|compile...\""
  (when android-file-name
    (let* ((file-sym (intern android-file-name android-file-prop-obarray))
           (build-dir (concat (get file-sym 'project-root) "build/"))
           (local-task (if (string= task "compile") "" task))
           (device (android-get-current-device))
           (cmake-command (concat
                           " -DDEVICE=" device
                           " -DARM_TARGET=\"" (car android-armeabi) "\""
                            " -DCMAKE_TOOLCHAIN_FILE="android-cmake-toolchain-file
                            " " (expand-file-name ".." build-dir)))
            (make-command (format "make %s" local-task)))
      (unless (file-directory-p build-dir) (make-directory build-dir))
      (setq default-directory build-dir)
      (call-process-shell-command (android-get-tool-path "cmake") nil nil nil cmake-command)
      (compile make-command))))

(defun android-make-command (&optional task args)
  "The interface to run command \"make install|uninstall|run|compile...\""
  (when android-file-name
    (let* ((default-directory
            (get (intern android-file-name android-file-prop-obarray) 'project-root))
          (device (android-get-current-device))
          (device-arg (when device (format " DEVICE=%s " device))))
      (compile (concat "make " device-arg (or task nil))))))

(defun android-debug-command (task &optional args)
  "The interface to run debug command"
  (when android-file-name
    (let ((default-directory
            (get (intern android-file-name android-file-prop-obarray) 'project-root)))
      (funcall (symbol-function (intern (concat "android-" task "-debug")))))))

(defun android-command (task)
  ""
  (let ((build-function
         (symbol-function (intern (concat "android-" (symbol-name android-build-tool) "-" task)))))
    (apply build-function nil)))

(defsubst android-run-activity ()
  "Function to launch activity from your device or emulator."
  (save-excursion
    (let* ((file-sym (intern android-file-name android-file-prop-obarray))
           (default-directory (get file-sym 'project-root))
           (device (android-get-current-device))
           (device-arg (if device (format " -s %s " device) "")))
      (android-launch-activity device-arg
       (list (get file-sym 'package)
             (get file-sym 'android:name))))))


(defalias 'android-ant-compile #'(lambda () (android-ant-command "compile")))
(defalias 'android-ant-install #'(lambda () (android-ant-command "install")))
(defalias 'android-ant-uninstall #'(lambda () (android-ant-command "uninstall")))
(defalias 'android-ant-clean #'(lambda () (android-ant-command "clean")))
(defalias 'android-ant-run #'(lambda () (android-run-activity)))

(defalias 'android-cmake-compile #'(lambda () (android-cmake-command "compile")))
(defalias 'android-cmake-install #'(lambda () (android-cmake-command "install")))
(defalias 'android-cmake-uninstall #'(lambda () (android-cmake-command "uninstall")))
(defalias 'android-cmake-clean #'(lambda () (android-cmake-command "clean")))
(defalias 'android-cmake-run #'(lambda () (android-cmake-command "run")))

(defalias 'android-make-compile #'(lambda () (android-make-command)))
(defalias 'android-make-install #'(lambda () (android-make-command "install")))
(defalias 'android-make-uninstall #'(lambda () (android-make-command "uninstall")))
(defalias 'android-make-run #'(lambda () (android-make-command "run")))
(defalias 'android-make-clean #'(lambda () (android-make-command "clean")))

(defalias 'android-ndk-compile #'(lambda () (interactive) (android-ndk-command "compile")))

(defalias 'android-compile #'(lambda () (interactive) (android-command "compile")))
(defalias 'android-install #'(lambda () (interactive) (android-command "install")))
(defalias 'android-uninstall #'(lambda () (interactive) (android-command "uninstall")))
(defalias 'android-run #'(lambda () (interactive) (android-command "run")))
(defalias 'android-clean #'(lambda () (interactive) (android-command "clean")))
(defalias 'android-debug-activity #'(lambda () (interactive) (android-debug-command "activity-jdb")))
(defalias 'android-debug-jni #'(lambda () (interactive) (android-debug-command "jni-gdb")))
(defalias 'android-debug-native #'(lambda () (interactive) (android-debug-command "native-gdb")))

(defalias 'android-adb-device-connect #'(lambda () (interactive) (android-adb-device-command "connect")))

(defalias 'android-adb-device-disconnect #'(lambda () (interactive) (android-adb-device-command "disconnect")))
(provide 'android-command)
