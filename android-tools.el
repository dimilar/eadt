;; File            : android-tools.el
;; 
;; Copyright (C) 2011 Free Software Foundation, Inc.
;; 
;; Author: Shulei Zhu <schuleichu@gmail.com>
;; Keywords: android-mode
;; Package: eadt
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; android-tools.el --- 
;; Created: Mi Sep 14 05:07:47 2011 (+0200)
;; Author: Shulei Zhu
;; 
;;; Code:

(defvar android-tools-alist
  '(("android" . "")
    ("adb" . "")
    ("gdb" . "")
    ("ant" . "")
    ("ndk-build" . "")
    ("emulator" . "")
    ("ddms" . "")
    ("cmake" . "")
    ("aapt" . "")
    ("gdbserver" . ""))
  "Variable to save the pathes of tools in common use,
CAR is the tool name, CDR is the path, which is set
by calling `android-set-tool-path'. Once one tool's path is set,
as you call the function `android-get-tool-path', the path will
 be returned.")

(defvar android-ndk-system
  (cond ((eq system-type "windows-nt") "windows")
        ((eq system-type "gnu/linux") "linux-x86")
        ((eq system-type "darwin") "")
        (t "linux-x86")))

(defcustom android-environment-variables 
 '(("ANDROID_SDK" "/opt/android-sdk" "")
   ("ANDROID_SDK_TOOLS" "/opt/android-sdk/tools" "android ddms emulator")
   ("ANDROID_SDK_PLATFORMTOOLS" "/opt/android-sdk/platform-tools" "adb aapt")
   ("ANDROID_NDK" "/opt/android-ndk" "ndk-build ndk-gdb ndk-stack")
   ("ANDROID_NDK_TOOLCHAIN_ROOT" "/opt/android-ndk-r6/\
toolchains/arm-linux-androideabi-4.4.3/prebuilt/" "gcc c++ gdb g++ ld ar as strip ranlib objcopy objdump nm")
   ("ANT_HOME" "/usr/share/ant/bin" "ant")
   ("BUILD_TOOL" "/usr/bin" "cmake make")
   ("DEBUG_TOOL" "/opt/android-ndk-r6/toolchains/arm-linux-androideabi-4.4.3/prebuilt" "gdbserver"))
  "A list to set some significat environment varaibles. Each element is a list which contains 3 elements:
the first is the name of the environment variable, the second is the value (path) of the varaible, while the
third one is the tools which can be found along the path. Although it is possible to find the tools' path witout
these varaibles, you are recommended to set them, for performance, as well as reliability."
  :group 'android-mode
  :type '(repeat
          (list
           :tag "Android Mode Enviroment"
           (string :tag "Environment Type") 
           (string :tag "Paths") 
           (string :tag "Tools"))))

(defun android-environment-add-directories ()
  "If failed to get the path of the tools along the `android-environment-variables'
it will search the tool in the following supplimentary pathes."
  (let ((dirs nil)
        (env nil))
    (push "/usr/bin" dirs)
    (push "/usr/local/bin" dirs)
    (push "~/android-sdk/tools" dirs)
    (push "~/android-sdk/platform-tools" dirs)
    (push "~/android-ndk" dirs)
  (when (setq env (getenv "ANDROID_SDK"))
    (push (concat env "/tools") dirs)
    (push (concat env "/platform-tools") dirs))
  (when (setq env (getenv "ProgramFiles"))
    (push (concat env "/Android/android-sdk/tools") dirs)
    (push (concat env "/Android/android-sdk/platform-tools") dirs)
    (push (concat env "/Android/android-ndk/") dirs)
    (push (concat env "/Android/android-ndk/toolchains/arm-linux-androideabi-4.4.3/prebuilt/linux-x86/bin") dirs)
    (push (concat env "/ant") dirs))
  (when (setq env (getenv "ANDROID_NDK"))
    (push (concat env "") dirs)
    (push (concat env "toolchains/arm-linux-androideabi-4.4.3/prebuilt") dirs))
  (when (setq env (getenv "ANT_DIR"))
    (push (concat env "/bin") dirs))
  (mapc #'(lambda (x) (push (concat "/opt/"  x "/tools") dirs))
          '("android-sdk"  "android-sdk-linux_x86" "android-sdk-mac_x86" "android-sdk-mac_86"))
  (mapc #'(lambda (x) (push (concat "/opt/"  x "/platform-tools") dirs))
          '("android-sdk"  "android-sdk-linux_x86" "android-sdk-mac_x86" "android-sdk-mac_86"))
  (mapc #'(lambda (x) (push (concat "/opt/"  x "") dirs))
          '("android-ndk"  "android-ndk-linux_x86" "android-ndk-mac_x86" "android-ndk-mac_86"))
  (mapc #'(lambda (x) (push (concat "/opt/"  x "/toolchains/arm-linux-androideabi-4.4.3/prebuilt") dirs))
          '("android-ndk"  "android-ndk-linux_x86" "android-ndk-mac_x86" "android-ndk-mac_86"))
  (mapc #'(lambda (x) (push (concat "/opt/"  x "/toolchains/arm-linux-androideabi-4.4.3/prebuilt") dirs))
          '("android-ndk"  "android-ndk-linux_x86" "android-ndk-mac_x86" "android-ndk-mac_86"))
  dirs))

(defun android-search-tool (files directories)
  "Recursively search a tool in those supplementary direcotries"
  (let (path)
    (if (null directories) nil
      (catch 'break
      (dolist (file files)
        (let ((tmp (concat (car directories) "/" file)))
          (when (file-regular-p tmp)
            (throw 'break tmp))))
      (android-search-tool files (cdr directories))))))

(defun android-set-tool-path (tool-name)
  "Function to set the pathes of those tools listed in `android-tools-alist'"
  (let* ((extention
         (if (eq system-type "windows-nt") '(".bat" ".exe") '("")))
         tool-path directories)
    (catch 'break
      (dolist (item android-environment-variables)
        (when (member tool-name (split-string (nth 2 item) " "))
          (let*
              ((fullname
                (if (string= (nth 0 item) "ANDROID_NDK_TOOLCHAIN_ROOT")
                    (concat "/" android-ndk-system "/bin/arm-linux-androideabi-" tool-name) tool-name))
               (files (mapcar
                  #'(lambda (x)
                      (let ((file (concat (nth 1 item) "/" fullname x)))
                        (if (file-regular-p file) (setq tool-path file) fullname))) extention)))
            (if (not tool-path)
                (progn
                  (setq directories (android-environment-add-directories))
                  (setq tool-path (android-search-tool files directories)))))
          (throw 'break tool-path))))
    (if tool-path (setcdr (assoc tool-name android-tools-alist) tool-path)
      (error (format "can not find tool %s" tool-name)))))


(defmacro android-get-tool-path (tool-name)
  "Macro to get one tool's full path, if not find in the `android-tools-alist',
search and set it."
  (let ((tool-path (cdr (assoc tool-name android-tools-alist))))
    (if (string= tool-path "")
        (android-set-tool-path tool-name)
      tool-path)))

(provide 'android-tools)
