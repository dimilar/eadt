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
(defvar android-minor-mode-menu nil)


(defvar android-target "android-8")
(defvar android-project-name nil)
(defvar android-native-project-type "c")
(defcustom android-build-tool 'ant
  ""
  :group 'android-mode
  :type '(string
	  (radio-button-choice
	  (item 'ant)
	  (item 'cmake)
	  (item 'make))))

(defvar android-mode-string nil "")
(defvar android-file-name nil)

(make-variable-buffer-local 'android-target)
(make-variable-buffer-local 'android-project-name)
(make-variable-buffer-local 'android-native-project-type)
(make-variable-buffer-local 'android-build-tool)
(make-variable-buffer-local 'android-mode-string)
(make-variable-buffer-local 'android-file-name)

(require 'easymenu)
(require 'xml)
(require 'tempo)
(require 'compile)


(defun android-minor-keymap ()
  (let ((keymap (make-sparse-keymap)))
    (easy-menu-define android-minor-mode-menu
      keymap
      "Menu used when Android-minor-mode is active."
      `("Android"
        "----"
        ("Project"
         ["New Android Project" android-project-new
          :help "Create a new android project using the \"android\" tool"]
         ["New Native Project" android-project-new-native
          :help "Create a native c/c++ project without android"]
         ["Update Android Project" android-project-update-proj
          :help "Upgrading a project from an older Android SDK, or from existing code"]
         ["Setup Libary Project" android-project-new-libproj
          :help "Creat a new library project using the \"android\" tool"]
         ["Reference Libary Project" android-project-update-libproj
          :help "Update the build properties of the library project"]
         ["Update Libary Project" android-project-ref-libproj
          :help "Add a reference to the library project"]
         )
        "----"
        ["NDK Build" android-ndk-compile
         :help "Generate and copy the libraries needed by your application to the proper location"
         :active t
         ]
        "----"
        ["Build" android-compile
         :help "Compile the source code of the project"
         :active (android-get-command-enable-state 'compile)]
        ["Install" android-install
         :help "Install the binary program or apk to your emulator"
         :active (android-get-command-enable-state 'install)]
        ["Run" android-run
         :active (android-get-command-enable-state 'run)         
         :help "Run the program or package on the emulator"]
        ["Clean " android-clean
         :active (android-get-command-enable-state 'clean)         
         :help "Get rid of all the objects files, executables or apk"]        
        ["Uinstall" android-uninstall
         :active (android-get-command-enable-state 'uninstall)         
         :help "Remove the binary program or apk from your emulator"]        
        "----"
        ["Debug Activity" android-debug-activity
         :active (android-get-command-enable-state 'debug-activity)
         :help "Call the jdb to debug the Android activity "]
        ["Debug Jni" android-debug-jni
         :active (android-get-command-enable-state 'debug-jni)
         :help "Execute Remote debugging the Java Native Interface using gdb"]
        ["Debug Native Program" android-debug-native
         :active (android-get-command-enable-state 'debug-native)
         :help "Debug the binary program of the native project"]
        "----"
        ("Switch Build Tool"
         ["Ant" (android-switch-build-tool "ant")
          :help "Use the \"ant\" as build tool"
          :active t :style radio :selected (eq android-build-tool 'ant)
          ]
         ["Cmake" (android-switch-build-tool "cmake")
          :help "Manage the project using \"cmake\", but still need call \"ant\" to build the project"
          :active t :style radio :selected (eq android-build-tool 'cmake)
          ]
         ["Make" (android-switch-build-tool "make")
          :help "Manage the native project using \"make\" "
          :active t :style radio :selected (eq android-build-tool 'make)
          ]
         )
        ;("Switch Native Project Type"
        ; ["C" (setq-default android-native-project-type "c")
        ;  :help "C program"
        ;  :active t :style radio :selected (string= android-native-project-type "c")
        ;  ]
        ; ["C++" (setq-default android-native-project-type "c++")
        ;  :help "C++ program"
        ;  :active t :style radio :selected (string= android-native-project-type "c++")
        ;  ])
        "----"
        ["Start Logcat" android-logcat
         :help "Launch the Android logging system"]
        ["Start Emulator" android-launch-emulator
         :help "Start the emulator with some Android Virtual Device"]
        ["Start DDMS" android-launch-ddms
         :help "Start the debugging tool DDMS (the Dalvik Debug Monitor Server)"]
        ["Reload targets" android-targets-reload
         :help "If some new targets was created, list them"]))
    (define-key keymap (read-kbd-macro "C-c C-c b") 'android-compile)    
    (define-key keymap (read-kbd-macro "C-c C-c c") 'android-clean)            
    (define-key keymap (read-kbd-macro "C-c C-c i") 'android-install)
    (define-key keymap (read-kbd-macro "C-c C-c u") 'android-uninstall)
    (define-key keymap (read-kbd-macro "C-c C-c l") 'android-logcat)
    (define-key keymap (read-kbd-macro "C-c C-c n") 'android-ndk-compile)
    (define-key keymap (read-kbd-macro "C-c C-c r") 'android-run)
    (define-key keymap (read-kbd-macro "C-c C-c d a") 'android-debug-activity)
    (define-key keymap (read-kbd-macro "C-c C-c d j") 'android-debug-jni)
    (define-key keymap (read-kbd-macro "C-c C-c d n") 'android-debug-native)
    (define-key keymap (read-kbd-macro "C-c C-c D") 'android-start-ddms)
    (define-key keymap (read-kbd-macro "C-c C-c e") 'android-start-emulator)    
    (define-key keymap (read-kbd-macro "C-c C-c p a") 'android-project-new)      
    (define-key keymap (read-kbd-macro "C-c C-c p n") 'android-project-new-native)      
    (define-key keymap (read-kbd-macro "C-c C-c p u") 'android-project-update-proj)   
    (define-key keymap (read-kbd-macro "C-c C-c p l") 'android-project-new-libproj)   
    (define-key keymap (read-kbd-macro "C-c C-c p r") 'android-project-ref-libproj)   
    (define-key keymap (read-kbd-macro "C-c C-c p U") 'android-project-update-libproj)
    (define-key keymap (read-kbd-macro "C-c C-c s t") 'android-project-switch-build-tool)
    keymap))

(defvar android-ndk-system
  (cond ((eq system-type "windows-nt") "windows")
        ((eq system-type "gnu/linux") "linux-x86")
        ((eq system-type "darwin") "")
        (t "linux-x86")))


(defun android-find-root (file)
  ""
  (locate-dominating-file file "AndroidManifest.xml"))

(defmacro string-trim (string)
  ""
  `(replace-regexp-in-string "\\(^[ \t]*\\|[ \t]*$\\)" "" ,string))


(require 'android-tools)
(require 'android-init)
(require 'android-command)
(require 'android-debug)
(require 'android-launch)
(require 'android-target)
(require 'android-project)
(require 'android-gen)


(defvar android-minor-mode-map (android-minor-keymap))

;;;###autoload
(define-minor-mode android-mode
  "Android application development minor mode."
  nil
  android-mode-string
  android-minor-mode-map)

;; (add-hook 'dired-mode-hook (lambda () (when (android-root) (android-mode t))))
(add-hook 'find-file-hook  'android-find-file-hook)
(add-hook 'dired-mode-hook  'android-find-file-hook)
(provide 'android)
