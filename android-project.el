;; File            : android-project.el
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
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; android-project.el --- 
;; Created: Mi Sep 14 05:07:30 2011 (+0200)
;; Author: Shulei Zhu
;; 
;;; Code:

(require 'android-gen)

(defmacro string-trim (string)
  ""
  `(replace-regexp-in-string "\\(^[ \t]*\\|[ \t]*$\\)" "" ,string))

(defconst android-project-management-type
  '((("create" . "project") (target name path activity package build-tool))
    (("update" . "project") (target path name))
    (("create" . "lib-project") (name target package path))
    (("update" . "lib-project") (path target))
    (("update" . "lib-project") (target path library))
    (("create" . "native-project") (target path name type build-tool))))

(defun android-project-sentinel-function (proc msg)
  (when (memq (process-status proc) '(exit signal))
    (if (equal 0 (process-exit-status proc))
        (message (substring (format "Successfully: %s" msg) 0 -1))
      (error (format "Project:%s" msg)))))

(defun android-project-create (type args n)
  "thisandthat."
  (let* (key value arg-string result project-root target
             build-tool build-tool-summary (type-command (car type))
             (type-name (cdr type)))
    (dolist (item args)
      (when item
        (setq key (car item))
        (setq value (cdr item))
        (catch 'continue
          (when (and (stringp value)
                     (not (string= (string-trim value) "")))
            ;; if build-tool is given, continue
            (when (eq key 'build-tool)
              (setq build-tool value)
              (throw 'continue nil))
            (when (eq key 'target)
              (setq value (nth 1 (split-string value " "))
                    target value))
            (when (eq key 'path)
              (if (string= (substring value -1) "/")
                  (setq project-root value)
                (setq project-root (concat value "/"))))
            (setq arg-string (concat arg-string " --" (symbol-name key) " " value))))))
    (setq result (condition-case err
        (call-process-shell-command
         (android-get-tool-path "android") nil nil nil
         type-command type-name arg-string)
      (error (format "Failed: %s" err))))
    (when (eq result 0)
      (when build-tool
        ;; default is build-tool
        (setq build-tool-summary build-tool)
        (cond ((string= build-tool "cmake")
               ;; (android-project-prop-reset
               ;;  project-root (list (cons 'build-tool "cmake")))
             ;; need to open a file to get the project's properties
             (let ((buf (find-file (concat project-root "AndroidManifest.xml"))))
               (save-excursion
                 (with-current-buffer buf
                   (condition-case  err
                     (progn 
                       (android-switch-build-tool "cmake")
                       (android-project-gen-file
                        (concat project-root "CMakeLists.txt")
                        (list 'android-gen-cmakelists-header
                              'android-gen-cmakelists-tail)
                        (list (cons 'android-project-name
                                    (concat (cdr (assq 'package args)) "/"
                                            (cdr (assq 'name args))))
                              (cons 'android-target target))))
                     (err (message (format "failed to generate the CMakeLists.txt: %s" (error-message-string err))))))
                 (kill-buffer buf))))
              ;; if not ant and cmake
              ((not (string= build-tool "ant"))
               (setq build-tool-summary "The build tool you\
 provided is not support for this project, use the default tool ant instead.")))
        (setq build-tool-summary
              (concat "Build tool: "
                      (propertize build-tool-summary 'face font-lock-warning-face))))
      (let* ((args (split-string arg-string " "))
             (summary (mapconcat #'(lambda (x)
                                     (if (string-match "^--\\(.+?\\)$" x)
                                         (concat (capitalize (match-string 1 x)) ":")
                                       (propertize x 'face font-lock-warning-face))) args " ")))
        (when (string= type-command "update")
          (android-project-set-prop project-root))
        (message (concat type-name " successfully " type-command "d (" summary " )"
                         build-tool-summary))))))
    ;; (set-process-sentinel
    ;;  (funcall
    ;;   'start-process-shell-command
    ;;   "*android project*" nil
    ;;   (format "%s %s %s %s" (android-get-tool-path "android") (car type) (cdr type) arg-string))
    ;;  'android-project-sentinel-function)))

(defmacro android-project-gen-file (file-path gen-fun var)
  `(and
   (or (not (file-exists-p ,file-path))
       (yes-or-no-p (format "%s exist, do you want to overwrite it? " ,file-path)))
   (save-excursion
     (android-gen-file ,file-path ,gen-fun ,var))))

;;;###autoload
(defun android-project-gen-cmakelists ()
  (interactive)
  (let* ((project-root (get (intern android-file-name android-file-prop-obarray) 'project-root))
         (cmakelists (concat project-root "CMakeLists.txt"))
         (project-sym (intern project-root android-project-prop-obarray))
         (target (get project-sym 'target))
         (project-name (concat (get project-sym 'package) "/"
                               (get project-sym 'android:name))))
    (android-project-gen-file
     cmakelists
     (list 'android-gen-cmakelists-header
           'android-gen-cmakelists-tail)
     (list (cons 'android-project-name project-name)
           (cons 'android-target target)))))

;;;###autoload
(defun android-project-switch-build-tool ()
  (interactive)
  (let* ((all-tools '(ant cmake make))
         (tools-excluding (delq android-build-tool all-tools))
         (build-tool (completing-read "Build tool: " (mapcar 'symbol-name all-tools) nil t
                                      (symbol-name (car tools-excluding))
                                      nil (symbol-name (car tools-excluding)))))
    (android-switch-build-tool build-tool)))

(defun android-project-create-1 (type args n)
  "thisandthat."
  (let* (key value project-root build-tool
             type manifest makein makefile cmakelists
             original-target target project-name result)
    (dolist (item args)
      (when item
        (setq key (car item)
              value (cdr item))
        (when (and (stringp value)
                   (not (string= (string-trim value) "")))
          (cond ((eq key 'path)
                 (if (not (file-directory-p value))
                     (make-directory value))
                 (if (string= (substring value -1) "/")
                     (setq project-root value)
                     (setq project-root (concat value "/")))
                 (setq manifest (concat project-root "AndroidManifest.xml")
                       makein (concat project-root "AndroidMakefile.in")
                       makefile (concat project-root "Makefile")
                       cmakelists (concat project-root "CMakeLists.txt")))
                ((eq key 'target)
                 (setq original-target value
                       target (nth 1 (split-string value " "))))
                ((eq key 'name)
                 (setq project-name value))
                ((eq key 'type)
                 (setq type value))
                ((eq key 'build-tool)
                 (setq build-tool value))))))
    (save-excursion
      (cond ((string= build-tool "cmake")
           (setq result
                 (and (android-project-gen-file
                       manifest (list 'android-gen-dummy-manifest)
                       (list (cons 'android-project-name project-name)
                             (cons 'android-native-project-type type)))
                      (android-project-gen-file
                       cmakelists (list 'android-gen-cmakelists-header
                                        'android-gen-cmakelists-native-tail)
                       (list (cons 'android-target target)
                             (cons 'android-project-name project-name)
                             (cons 'android-native-project-type type)))))
             (android-project-prop-reset
              project-root (list (cons 'build-tool "cmake"))))
          (t (setq result (and
                           (android-project-gen-file
                            manifest (list 'android-gen-dummy-manifest)
                            (list (cons 'android-project-name project-name)
                                  (cons 'android-native-project-type type)))
                           (android-project-gen-file
                            makein (list 'android-gen-makein-buffer)
                            (list (cons 'android-target target)
                                  (cons 'android-native-project-type type)))
                           (android-project-gen-file
                            makefile (list 'android-gen-makefile-buffer)
                            (list (cons 'android-project-name project-name)
                                  (cons 'android-native-project-type type)))))
             (unless (string= build-tool "make")
                 (setq build-tool "The build tool you\
 provided is not support for this project, use the default tool make instead."))
             (android-project-prop-reset
              project-root (list (cons 'build-tool "make"))))))
    (if result
        (progn 
            ;; change the property `program of the new project
            (android-project-set-prop project-root) 
            (message
             (apply 'format
                    "successfully created a native project (Path: %s Name: %s Type: %s Target: %s Build Tool: %s)"
                    (mapcar #'(lambda (x) (propertize x 'face font-lock-warning-face))
                            (list project-root project-name type original-target build-tool)))))
        (message "Failed to create the project."))))

(defmacro android-project-interactive-form (options)
  `(let ((targets-list (or android-targets-list (android-get-targets-list)))
         (tools (list "ant" "cmake" "make"))
         (types (list "c" "c++"))
         (project-root (when android-file-name (get (intern android-file-name android-file-prop-obarray) 'project-root))))
     (list
      `(,@(list
         (when (memq 'target ,options)
           (cons 'target
                 (read-from-minibuffer 
                  "target: " 
                  (car targets-list) nil nil 'targets-list (car targets-list))))
         (when (memq 'path ,options)
           (cons 'path
                 (read-file-name 
                  "path: " 
                  project-root nil
                  nil nil 'file-directory-p)))
         (when (memq 'name ,options)
           (cons 'name
                 (read-from-minibuffer "name: " nil nil nil)))
         (when (memq 'activity ,options)
           (cons 'activity
                 (read-from-minibuffer "activity: " nil nil nil)))
         (when (memq 'type ,options)
           (cons 'type
                 (read-from-minibuffer "Project Type(c/c++): " (car types) nil nil 'types (car types))))
                 ;; (read-from-minibuffer "Project Type(c/c++):" nil nil nil)))
         (when (memq 'package ,options)
           (cons 'package
                 (read-from-minibuffer "Package: " nil nil nil)))
         (when (memq 'library ,options)
           (cons 'library
                 (read-from-minibuffer "Library: " nil nil nil)))
         (when (memq 'build-tool ,options)
           (cons 'build-tool
                 (read-from-minibuffer "Build Tool: " (car tools) nil nil 'tools (car tools)))))))))

(defun android-project-refresh ()
  ""
  
  )

(defun android-project-command (args n)
  ""
  (let* ((type (car (nth n android-project-management-type))))
    (if (string= (cdr type) "native-project")
        (funcall 'android-project-create-1 type args n)
    (funcall 'android-project-create type args n))))

;;;###autoload
(defun android-project-new (args)
  ""
  (interactive (android-project-interactive-form 
                (nth 1 (nth 0 android-project-management-type))))
  (android-project-command args 0))

;;;###autoload
(defun android-project-update-proj (args)
  ""
  (interactive (android-project-interactive-form
                (nth 1 (nth 1 android-project-management-type))))
  (android-project-command args 1))

;;;###autoload
(defun android-project-new-libproj (args)
  ""
  (interactive (android-project-interactive-form
                (nth 1 (nth 2 android-project-management-type))))
  (android-project-command args 2))

;;;###autoload
(defun android-project-update-libproj (args)
  ""
  (interactive (android-project-interactive-form
                (nth 1 (nth 3 android-project-management-type))))
  (android-project-command args 3))

;;;###autoload
(defun android-project-ref-libproj (args)
  ""
  (interactive (android-project-interactive-form
                (nth 1 (nth 4 android-project-management-type))))
  (android-project-command args 4))

;;;###autoload
(defun android-project-new-native (args)
  ""
  (interactive (android-project-interactive-form 
                (nth 1 (nth 5 android-project-management-type))))
  (android-project-command args 5))

(provide 'android-project)
