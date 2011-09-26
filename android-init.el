;; File            : android-init.el
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
;;; android-init.el --- 
;; Created: Mi Sep 14 05:06:48 2011 (+0200)
;; Author: Shulei Zhu
;; 
;;; Code:
;; (require 'android)

(eval-when-compile (require 'cl))
(require 'xml)
(require 'easymenu)
(require 'android-tools)


(defvar android-file-name nil)
(make-variable-buffer-local 'android-file-name)

(defvar android-mode-string nil "")
(make-variable-buffer-local 'android-mode-string)

(defcustom android-build-tool 'ant
  ""
  :group 'android-mode
  :type '(string
	  (radio-button-choice
	  (item 'ant)
	  (item 'cmake)
	  (item 'make))))
(make-variable-buffer-local 'android-build-tool)

(defvar android-project-prop-obarray (make-vector 10 0)
  "Obarray for per-project properites")

(defvar android-file-prop-obarray (make-vector 20 0)
  "Obarray for per-file properties")

;; TODO: if a user creates a item including blank around the "="
(defvar android-prop-regexp
  "^\\(target\\|sdk.dir\\|source.dir\\|out.dir\\|key.store\
\\|key.alias\\|android.library.reference.[1-3]\\)=\\(.*\\)"
  "The regular experssion used to parse the prop files of android
project: default.properties local.properties build.properites...")

(defvar android-prop-makefile-regexp
  "^\\(program\\)[ ]*?:=[ ]*\\(.*\\)[^ ]*?"
  "The regexp used to retrieve the program name from the Makefile, note
it is only usful for native project")

(defcustom android-device nil
  "User can use it to specific a device as the default one.
If it is nil, and some other devices are available, user can
select one from the menu. Note it is a buffer local variable."
  :group 'android-mode
  :type '(string :tag "default device")
  :set '(lambda (sym val) (set-default sym val)))
(make-variable-buffer-local 'android-device)

(defvar android-devices-alist nil
  "A alist to store all devices/emulators existing on your machine
or in your network environment. Each element consists of a device (CAR)
and its state (CDR): offline|device|bootloader.")


(defvar android-minor-mode-menu nil)
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
         :help "Install the binary program or apk to your device/emulator"
         :active (android-get-command-enable-state 'install)]
        ["Run" android-run
         :active (android-get-command-enable-state 'run)         
         :help "Run the program or package on the device/emulator"]
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
         :help "Remotely debug the Java Native Interface with gdb"]
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

(defun android-get-devices()
  "A function used to fetch all devices available on your machine or around it
by applying the command `adb devices'. The result is stored in the
android-devices-alist"
  (let* ((command (format "%s devices" (android-get-tool-path "adb")))
         (output (shell-command-to-string command))
         (start 0) device state pair)
    (while (string-match "\n\\(.*\\)\t\\(offline\\|device\\)" output start)
      (setq device (match-string 1 output)
            state (match-string 2 output)
            pair (assoc device android-devices-alist))
      (if pair
          (setcdr pair state)
        (setq android-devices-alist
              (append android-devices-alist (list (cons device state)))))
      (setq start (match-end 2)))))

(defun android-get-current-device ()
  "Get the device which is associated with the current buffer, if the buffer belongs to some android project,
and a device is activated, retrun the device name, otherwise return nil."
  (let ((device (get (intern android-file-name android-file-prop-obarray) 'device)))
    (when (and device (assoc (car device) android-devices-alist))
      (when (and (not (string= (cdr device) "device"))
                 (not (string= (cdr device) "error: unknown host service")))
        (call-process-shell-command (android-get-tool-path "adb") nil nil nil
                                    (format " -s %s wait-for-device" (car device))))
      (car device))))

(defsubst android-get-device-state ()
  "Get the state of the device specified in the property :device of the current
file. However, for those devices connected through the internet,
the state obtained by executing `adb -s YOUR-DEVICE-NAME  get-state' is:
\"error: unknown host service\""
  (substring (shell-command-to-string
              (format "%s -s %s get-state"
                      (android-get-tool-path "adb") android-device)) 0 -1))

(defun android-refresh-device-menu ()
  "each project has its own device menu whose visibility is determined by
the project root of the current buffer."
  ;; TODO: if user does not customize a default device,
  ;;       after connected to a new device (before there is no device available)
  ;;       how to update the property "device" of all open files
  (let* ((file-sym (intern android-file-name android-file-prop-obarray))
         (proj-root (get file-sym 'project-root))
         (file-device (car (get file-sym 'device))))
    ;; if android-devices-alist is  nil, try to fill it
    (or android-devices-alist (android-get-devices))
    ;; if user define a device, while the device is not
    ;; in the alist, append the device to the alist
    (when (and android-device
               (not (assoc android-device android-devices-alist)))
      (setq android-devices-alist
            (append android-devices-alist
                    (list (cons android-device (android-get-device-state))))))
    (dolist (dv android-devices-alist)
      (define-key android-minor-mode-menu (vector (intern (concat proj-root (car dv))))
        `(menu-item (concat (car ',dv) ": " (cdr ',dv)) (lambda () (interactive) (android-switch-device (car ',dv)))
                    :button (:radio . (string= (car ',dv) ,file-device))
                    :visible (string= (get (intern android-file-name android-file-prop-obarray)
                                           'project-root) ',proj-root))))
    (when android-devices-alist
      (define-key android-minor-mode-menu [separator-devices]
        '(menu-item "--")))))

(defun android-switch-device(device)
  "If multiple devices exist, when clicking the corresponding menu item,
set the properity :DEVICE of the current file and the project which this file belongs to,
as well as of all open files under the same project. In the end, redraw the device menu."
  (let (buf (device-cons (cons device (android-get-device-state)))
            (project-root (get (intern android-file-name android-file-prop-obarray)
                               'project-root)))
    (unless (string= device android-device)
      (loop for sym across android-file-prop-obarray
            do
            (when (and (symbolp sym)
                     (get sym 'project-root)
                     (string= (get sym 'project-root)
                              project-root)
                     (setq buf (get-file-buffer (symbol-name sym))))
              (with-current-buffer buf
                (setq android-device device)
                (put sym 'device device-cons))))
      (android-project-prop-reset
       project-root (list (cons 'device device-cons))))
    (android-refresh-device-menu)))


(defvar android-command-type
  '(compile install uninstall
    clean recompile run debug-activity debug-jni debug-native)
  "some commands executed by clicking the menu item. This varibale
is used to control the enabilities of these commands on the menu")

(defsubst android-get-command-enable-state (command)
  "inline function to get the enability of the COMMAND"
  (cdr (assq command
             (get (intern android-file-name android-file-prop-obarray)
                  'command-enable))))

(defun android-set-command-visibility (tool-sym)
  "thisandthat."
  (let ((command-enability (mapcar #'(lambda (x) (cons x t)) android-command-type)))
    (cond ((eq tool-sym 'ant)
         ;; (aset command-enable-list 5 nil)
         (setcdr (assq 'debug-native  command-enability) nil))
        ;; ((string= tool "cmake")
        ;;  (aset command-enable-list 5 nil))
        ((eq tool-sym 'make)
         (setcdr (assq 'debug-activity  command-enability) nil)
         (setcdr (assq 'debug-jni command-enability) nil)))
  command-enability))

(defsubst android-set-mode-string ()
  "Set the mode string of the android minor mode. The string consists of
\"Android\" and the name of the build tool. Note the latter is buffer local varaible"
  (unless (or (member 'android-mode-string global-mode-string)
                 (string= 'jde-mode "jde-mode"))
       (setq global-mode-string (append global-mode-string
                                        (list 'android-mode-string))))
     (setq android-mode-string (format " Android(%s)"  (symbol-name android-build-tool))))

(defun android-switch-build-tool (tool)
  "When switching to another building tool (on of ant, cmake and make ...)
modify the property :build-tool of the current file and the project which this file belongs to,
as well as of all open files under the same project. Furthermore, it is required to
update the mode string and show the current build tool."
  (let* (buf
         (tool-sym (intern tool))
         (command-enability (android-set-command-visibility tool-sym))
        (project-root (get (intern android-file-name android-file-prop-obarray) 'project-root)))
    (if (eq tool-sym android-build-tool)
        (android-set-mode-string)
      (loop for sym across android-file-prop-obarray
            do (when (and
                      (symbolp sym)
                      (get sym 'build-tool)
                      (string= (get sym 'project-root) project-root)
                      (setq buf (get-file-buffer (symbol-name sym))))
                 (with-current-buffer buf
                   (setq android-build-tool tool-sym)
                   (put sym 'build-tool tool)
                   (put sym 'command-enable command-enability)
                   (android-set-mode-string))))
      (android-project-prop-reset
       project-root
       (list (cons 'build-tool tool)
             (cons 'command-enable command-enability))))))

(defcustom android-armeabi (list "armeabi-v7a with NEON")
  "Type of floating point support, please use
`adb -s ANDROID_SERIAL shell cat /proc/cpuinfo' to check which type your device/emulator
supports. The default value is \"android-v7a with NEON\""
  :group 'android-mode
  :type '(list
	  (radio-button-choice
	  (item "armeabi")
	  (item "armeabi-v7a with NEON")
	  (item "armeabi-v7a with VFPV3"))))

(defun android-get-abi ()
  "if a device is available, try to determin the abi and abi2.
return a cons: CAR is the abi or nil, while CDR is abi2 or nil"
  (let* ((device (android-get-current-device))
        (device-arg (if device (format " -s %s " device) ""))
        (buffer "*android getprop*")
        abi-first abi-second)
    (call-process-shell-command
     (android-get-tool-path "adb")
     nil buffer nil (format " %s shell getprop" device-arg))
    (save-excursion
      (save-window-excursion
        (with-current-buffer buffer
          (goto-char (point-min))
          (if (re-search-forward "\\\[ro\.product\.cpu\.abi\\\]: \\\[\\(.*?\\)\\\]" nil t)
              (setq abi-first (buffer-substring (match-beginning 1) (match-end 1))))
          (if (re-search-forward "\\\[ro\.product\.cpu\.abi2\\\]: \\\[\\(.*?\\)\\\]" nil t)
              (setq abi-second (buffer-substring (match-beginning 1) (match-end 1)))))
        (kill-buffer buffer)))
    (cons abi-first abi-second)))

(defun android-parse-prop (file regexp)
  "Parse the properities in a given properties-file generated by android tools, e.g.
default.properties, local.properties, build.properties and so on.
Return a ALIST consisting of the properties of a android project"
  (let* ((filename-without-path (car (last (split-string file "/"))))
         (buffer (concat "*android-mode-" filename-without-path ))
         (buf (get-buffer-create buffer))
         properties)
    (with-current-buffer buf
      (delete-region (point-min) (point-max))
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq properties (append properties (list (cons (match-string 1) (match-string 2))))))
      (kill-buffer buf)
      ) properties))

(defun android-parse-manifest (file)
  "Pasrse the AndroidManifest.xml and return some useful information:
package-name, version-code, version-name, program-name"
  (let ((node (car (xml-parse-file file)))
        (package-name nil) (program-name nil)
        (version-code nil) (version-name nil))
    (setq package-name (xml-get-attribute node 'package))
    (setq version-code (xml-get-attribute node 'android:versionCode))
    (setq version-name (xml-get-attribute node 'android:versionName))
    (setq program-name (xml-get-attribute (assoc 'activity (assoc 'application (cdr node))) 'android:name))
    (setq program-name (replace-regexp-in-string "\\(^[\.]*\\)" "" program-name))
    (list (cons "package"  package-name)
          (cons "android:name"  program-name)
          (cons "android:versionName" version-name)
          (cons "android.versionCode" version-code))))


(defun android-project-set-prop (project-root)
  "Set the properties of a given Android Project. A symbol with the name of
the project root is stored in android-project-prop-obarray."
  (let ((prop-list))
    (when (file-directory-p project-root)
      (let ((proj-sym (intern project-root android-project-prop-obarray))
            (manifest-file (concat project-root "AndroidManifest.xml"))
            (local-prop-file (concat project-root "local.properties"))
            (default-prop-file (concat project-root "default.properties"))
            (build-prop-file (concat project-root "build.properties"))
            (makein (concat project-root "AndroidMakefile.in"))
            (makefile (concat project-root "Makefile"))
            (cmakelists (concat project-root "CMakeLists.txt"))
            build-tool)
        (put proj-sym 'project-root project-root)
        (put proj-sym 'command-enable (mapcar #'(lambda (x) (cons x t)) android-command-type))
        (put proj-sym 'device
             (or (if android-device
                     (cons android-device
                           (android-get-device-state)))
                   (progn (unless android-devices-alist (android-get-devices)) (car android-devices-alist))))
        (when (file-regular-p manifest-file)
          (setq prop-list (append prop-list (android-parse-manifest manifest-file))))
        (when (file-regular-p local-prop-file)
          (setq prop-list (append prop-list (android-parse-prop local-prop-file android-prop-regexp))))
        (when (file-regular-p default-prop-file)
          (setq prop-list (append prop-list (android-parse-prop default-prop-file android-prop-regexp))))
        (when (file-regular-p build-prop-file)
          (setq prop-list (append prop-list (android-parse-prop build-prop-file android-prop-regexp))))
        (cond ((file-regular-p makein)
               (setq prop-list
                     (append prop-list
                             (nconc (list (cons "build-tool" "make")
                                          (cons "native" "on")) ;; AndroidMakefile.in 
                                    (when (file-regular-p makefile)
                                      (android-parse-prop makefile android-prop-makefile-regexp))))
                     build-tool 'make))
              ((file-regular-p cmakelists)
               (setq prop-list (append prop-list (list (cons "build-tool" "cmake")))
                     build-tool 'cmake))
              (t (setq prop-list (append prop-list (list (cons "build-tool" "ant")))
                       build-tool 'ant)))
        (put proj-sym 'command-enable (android-set-command-visibility build-tool))
        (dolist (prop prop-list)
          (let* ((prop-sym (intern (car prop)))
                 (new-prop (cdr prop))
                 (old-prop (get proj-sym prop-sym)))
            (unless (string= new-prop old-prop)
              (put proj-sym prop-sym new-prop))))))))



(defun android-file-clear-prop (file)
  "Clear all properties related to project."
  (setplist (intern file android-file-prop-obarray) nil))


(defun android-find-root (file)
  ""
  (locate-dominating-file file "AndroidManifest.xml"))

(defun android-file-set-prop (file)
  "Determine the project which the file belongs to, then copy
the symbol-plist of the project as the properties of the file."
  (let* (project-sym
         (dir (android-find-root file)))
    (when dir
      (setq project-sym (intern dir android-project-prop-obarray))
      (unless (symbol-plist project-sym)
        (android-project-set-prop dir))
      (setplist (intern file android-file-prop-obarray)
                (symbol-plist project-sym)))))

(defun android-project-prop-reset (project-root prop-list)
  "If one or more properties of the project were changed, set it to the new one(s).
Usually, it is also necessary to modify the corresponding proerties of all open files under this project."
  (let (sym value)
    (dolist (prop prop-list)
      (setq sym (car prop)
            value (cdr prop))
      ;; (put  (intern file-name android-file-prop-obarray) sym value)
      ;; (setq proj-root (get (intern file-name android-file-prop-obarray) 'project-root))
      (put (intern project-root android-project-prop-obarray) sym value))))


(defvar android-target "android-8")
(make-variable-buffer-local 'android-target)


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

;;;###autoload
(defun android-targets-reload ()
  ""
  (interactive)
  (setq android-targets-all-alist nil
        android-targets-available-alist nil
        android-targets-list nil)
  (android-list-targets))

(provide 'android-init)
