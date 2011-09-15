;; File            : android-template.el
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
;;; android-template.el --- 
;; Created: Mi Sep 14 05:08:38 2011 (+0200)
;; Author: Shulei Zhu
;; 
;;; Code:



(defcustom android-force-arm nil
  ""
  :group 'android-mode
  :type '(const :tag "force arm or not"))

(defcustom android-cmake-include-file "/zleinter/emax/android/android.cmake"
  ""
  :group 'android-mode
  :type 'string
  )

(defcustom android-cmake-toolchain-file "/zleinter/emax/android/android.toolchain.cmake"
  ""
  :group 'android-mode
  :type 'string
  )

(defun android-gen-file (file-name gen-fun &optional vars)
  "thisandthat."
  ;;; ?`find-file-literally'
  (let ((buf (find-file file-name)) local-variable)
      (with-current-buffer buf
        (when vars
          (dolist (var vars)
            (cond
             ((eq (car var) 'android-target)
              (setq android-target (cdr var)))
             ((eq (car var) 'android-project-name)
              (setq android-project-name (cdr var)))
             ((eq (car var) 'android-native-project-type)
              (setq android-native-project-type (cdr var))))))
        (delete-region (point-min) (point-max))
        (dolist (fun gen-fun)
          (apply fun nil))
        (save-buffer file-name)
        (kill-buffer buf))
      (if (file-exists-p file-name) file-name nil)))

(defun android-gen-read-template (strings)
  "Converts an autocode template represented as a list
of strings to a list of Lisp objects as required by
tempo."
  (let ((template-string "")
	(n (length strings))
	(i 0))
    (while (< i n)
      (setq template-string
	    (concat template-string (nth i strings) "\n"))
      (setq i (1+ i)))
    (setq template-string
	  (concat "'(" template-string ")"))
    (eval (car (read-from-string template-string)))))

(defcustom android-dummy-manifest-template
  (list
"\"<?xml version=\\\"1.0\\\" encoding=\\\"utf-8\\\"?>\" 'n"
"\"<manifest xmlns:android=\\\"http://schemas.android.com/apk/res/android\\\"\" 'n"
"\"      package=\\\"\\\"\"'n"
"\"      android:versionCode=\\\"1\\\"\"'n"
"\"      android:versionName=\\\"1.0\\\">\"'n"
"\"    <application android:label=\\\"@string/app_name\\\" android:icon=\\\"@drawable/icon\\\">\"'n"
"\"        <activity android:name=\\\"\" android-project-name \"\\\"\"'n"
"\"                  android:label=\\\"@string/app_name\\\">\"'n"
"\"            <intent-filter>\"'n"
"\"                <action android:name=\\\"android.intent.action.MAIN\\\" />\"'n"
"\"                <category android:name=\\\"android.intent.category.LAUNCHER\\\" />\"'n"
"\"            </intent-filter>\"'n"
"\"       </activity>\"'n"
"\"    </application>\"'n"
"\"</manifest>\"'n"
"\"<!-- DON'T DELETE ME!!! -->\" 'n"
"\"<!-- This file is automatically generated by Emacs Android Mode -->\"'n"
"\"<!-- For native project, it is a dummy file and is used as the sentinel for android mode -->\"")
  ""
  :group 'android-mode
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'android-gen-dummy-manifest
	    (tempo-define-template
	     "android-manifest.xml"
	     (android-gen-read-template val)
	     nil
	     "insert into AndroidManifest.xml."))
	  (set-default sym val)))

(defcustom android-cmakelist-header-template
  (list 
   "\"################################################################################\" 'n"
   "\"#\" 'n"
   "\"#  CMakeLists.txt for Android\" 'n"
   "\"#\" 'n"
   "\"# Author: \" user-full-name  'n"
   "\"################################################################################\" 'n"
   "\"\" 'n"
   "\"# Common Properties\" 'n"
   "\"CMAKE_MINIMUM_REQUIRED( VERSION 2.6)\" 'n"
   "\"if(NOT CMAKE_TOOLCHAIN_FILE)\" 'n"
   "\"    default_action()\" 'n"
   "\"endif(NOT CMAKE_TOOLCHAIN_FILE)\" 'n"
   "\"set( CMAKE_COLOR_MAKEFILE ON)\" 'n"
   "\"set(ANDROID_SDK_TARGET \\\"\" android-target \"\\\") \" 'n"
   "\"set(android_dependencies)\"'n"
   "\"include(\\\"\"android-cmake-include-file \"\\\")\" 'n"
   "\"set(CMAKE_ALLOW_LOOSE_LOOP_CONSTRUCTS true)\" 'n"
   "\" include_directories(SYSTEM \\\"$\{ANDROID_NDK_TOOLCHAIN_ROOT\}/user/include\\\" )\" 'n"
   "\"if(WITH_OPENCV)\" 'n"
   "\"    opencv_settings()\" 'n"
   "\"endif(WITH_OPENCV)\" 'n'n"
   "\"if(DEFINED DEVICE)\" 'n"
   "\"  set(DEVICE \\\"-s\\\" \\\"${DEVICE}\\\")\" 'n"
   "\"else()\" 'n"
   "\"  set(DEVICE \\\"\\\")\" 'n"
   "\"endif()\" 'n"
   "\"set(DEVICE_ARG ${DEVICE})\" 'n'n"
   "\"set(LINKER_LIBS log dl m)\" 'n"
   "\"find_android_sdk()\" 'n"   
   )
  ""
  :group 'android-mode
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'android-gen-cmakelists-header
	    (tempo-define-template
	     "android-cmakelist-header"
	     (android-gen-read-template val)
	     nil
	     "insert cmakelists header buffer."))
	  (set-default sym val)))

(defcustom android-cmakelist-tail-template
  (list 
   "\"set(ANDROID_PROJECT_NAME \\\"\" android-project-name \"\\\") \" 'n"
   "\"string(REGEX REPLACE \\\"^(.+)/\\\\\\\\.*(.+)$\\\" \\\"\\\\\\\\1\\\" package_name  \\\"${ANDROID_PROJECT_NAME}\\\")\" 'n"
   "\"string(REGEX REPLACE \\\"^(.+)/\\\\\\\\.*(.+)$\\\" \\\"\\\\\\\\2\\\" program_name  \\\"${ANDROID_PROJECT_NAME}\\\")\" 'n"   
   "\"add_android_project(\\\"$\{program_name}\\\" \\\"$\{CMAKE_CURRENT_SOURCE_DIR\}/\\\" \" (or (car (android-get-abi)) (car (split-string (car android-armeabi) " "))) \" )\" 'n"
   "\"add_custom_target(install \" 'n"
   "\"   COMMAND ${ADB_EXECUTABLE} ${DEVICE_ARG} -e install -r \\\"$\{CMAKE_BINARY_DIR\}/bin/$\{program_name\}.apk\\\"\" 'n"
   "\"   DEPENDS $\{program_name\}_android_project\" 'n"
   "\")\" 'n"
   "\"add_custom_target(uninstall \" 'n"
   "\"   COMMAND ${ADB_EXECUTABLE} ${DEVICE_ARG} -e uninstall \\\"$\{package_name\}\\\"\" 'n"
   "\")\" 'n"
   "\"add_custom_target(run \" 'n"
   "\"   COMMAND ${ADB_EXECUTABLE} ${DEVICE_ARG} shell am start -n \\\"${package_name}/.${program_name}\\\"\" 'n"
   "\")\" 'n"   
   )
  ""
  :group 'android-mode
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'android-gen-cmakelists-tail
	    (tempo-define-template
	     "android-cmakelists-tail"
	     (android-gen-read-template val)
	     nil
	     "insert cmakelist tail buffer."))
	  (set-default sym val)))


(defcustom android-cmakelist-native-tail-template
  (list 
   "\"set(program  \\\"\" android-project-name \"\\\") \" 'n"
   "\"set(INSTALL_DIR  \\\"/data/tmp\\\") \" 'n"
   "\"ADD_EXECUTABLE(\\\"${program}\\\" \\\"./${program}.\"(if (string= android-native-project-type \"c++\") (insert \"cpp\") (insert \"c\"))\"\\\") \" 'n"
   "\"# ADD_DEPENDENCIES(\\\"${program}\\\" ${android_dependencies}) \" 'n"
   "\"TARGET_LINK_LIBRARIES(\\\"${program}\\\" ${LINKER_LIBS} ${android_dependencies}) \" 'n"
   "\"set_target_properties(\\\"${program}\\\" PROPERTIES OUTPUT_NAME \\\"${program}\\\" RUNTIME_OUTPUT_DIRECTORY \\\"${EXECUTABLE_OUTPUT_PATH}\\\")\" 'n"
   "\"add_custom_target(install \" 'n"
   "\"   COMMAND ${ADB_EXECUTABLE} ${DEVICE_ARG} push \\\"${EXECUTABLE_OUTPUT_PATH}/${program}\\\" \\\"${INSTALL_DIR}/${program_name}\\\"\" 'n"
   "\"   DEPENDS ${program} \"'n"
   "\")\" 'n"
   "\"add_custom_target(uninstall \" 'n"
   "\"   COMMAND ${ADB_EXECUTABLE} ${DEVICE_ARG} shell rm \\\"${INSTALL_DIR}/${program}\\\"\" 'n"
   "\")\" 'n"
   "\"add_custom_target(run \" 'n"
   "\"   COMMAND ${ADB_EXECUTABLE} ${DEVICE_ARG} shell \\\"${INSTALL_DIR}/${program}\\\"\" 'n"
   "\")\" 'n"   
   )
  ""
  :group 'android-mode
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'android-gen-cmakelists-native-tail
	    (tempo-define-template
	     "android-cmakelist-native-tail"
	     (android-gen-read-template val)
	     nil
	     "insert cmakelist native tail buffer."))
	  (set-default sym val)))


(defcustom android-makein-template
  (list
   "\"################################################################################\" 'n"
    "\"#\" 'n"
    "\"#  Makefile for Android\" 'n"
    "\"#\" 'n"
    "\"# Author: \" user-full-name  'n"
    "\"################################################################################\" 'n"
    "\"\" 'n"
    "\"ANDROID_API_LEVEL := \" (car (last (split-string android-target \"-\"))) 'n"
    "\"ANDROID_NDK := \" (nth 1 (assoc \"ANDROID_NDK\" android-environment-variables)) 'n"
    "\"ANDROID_SDK := \" (nth 1 (assoc \"ANDROID_SDK\" android-environment-variables)) 'n"
    "\"NDK_SYSTEM := \" android-ndk-system 'n"
    "(insert \"ANDROID_NDK_TOOLCHAIN_ROOT := ${ANDROID_NDK}/toolchains/arm-linux-androideabi-4.4.3/prebuilt/${NDK_SYSTEM}\\n\")"
    "(cond
((string= android-native-project-type \"c\")
 (insert \"CC := ${ANDROID_NDK_TOOLCHAIN_ROOT}/bin/arm-linux-androideabi-gcc\\n\"))
((string= android-native-project-type \"c++\")
 (insert \"CC := ${ANDROID_NDK_TOOLCHAIN_ROOT}/bin/arm-linux-androideabi-g++\\n\")))"
"\"AR := ${ANDROID_NDK_TOOLCHAIN_ROOT}/bin/arm-linux-androideabi-ar\" 'n"
"\"NM := ${ANDROID_NDK_TOOLCHAIN_ROOT}/bin/arm-linux-androideabi-nm\" 'n"
"\"LINKER := ${ANDROID_NDK_TOOLCHAIN_ROOT}/bin/arm-linux-androideabi-ld\" 'n"
"\"OBJCOPY :=${ANDROID_NDK_TOOLCHAIN_ROOT}/bin/arm-linux-androideabi-objcopy\" 'n"
 "\"OBJDUMP :=${ANDROID_NDK_TOOLCHAIN_ROOT}/bin/arm-linux-androideabi-objdump\" 'n"
 "\"STRIP := ${ANDROID_NDK_TOOLCHAIN_ROOT}/bin/arm-linux-androideabi-stip\" 'n"
 "\"RANLIB := ${ANDROID_NDK_TOOLCHAIN_ROOT}/bin/arm-linux-androideabi-ranlib\" 'n"
 "\"GDB :=  ${ANDROID_NDK_TOOLCHAIN_ROOT}/bin/arm-linux-androideabi-gdb\"'n"
 "\"DEBUG = -g\" 'n"
 "\"CFLAGS := -Wall -fPIC -DANDROID -Wno-psabi -fsigned-char -fno-short-enums\" 'n"
 "(if android-force-arm
   (progn (cond ((string= android-native-project-type \"c++\")
   (insert \"CFLAGS += -marm\\n\"))
    ((string= android-native-project-type \"c\")  (insert \"CFLAGS += -marm\\n\"))))
  (progn (cond ((string= android-native-project-type \"c++\")
    (insert \"CFLAGS += -mthumb\\n\"))
    ((string= android-native-project-type \"c\") (insert \"CFLAGS += -mthumb\\n\")))))"
 "\"ARMEABI := \" (car (split-string (car android-armeabi) \" \")) 'n"
 "(let ((eabi (car android-armeabi)))
(when (string= (car (split-string eabi \" \")) \"armeabi-v7a\")
  (insert \"MAKE_SYSTEM_PROCESSOR = armv7-a\\n\")
  (cond ((string= android-native-project-type \"c++\")
         (insert \"CFLAGS += -march=armv7-a -mfloat-abi=softfp\\n\"))
        ((string= android-native-project-type \"c\")
         (insert \"CFLAGS += -march=armv7-a -mfloat-abi=softfp\\n\"))))
(cond ((string= eabi \"armeabi\")
       (insert \"ARMEABI_NDK_NAME := armeabi\\n\"))
      ((string= eabi \"armeabi-v7a\")
       (insert \"ARMEABI_NDK_NAME := armeabi-v7a\\n\"))
      ((string= eabi \"armeabi-v7a with NEON\")
       (cond ((string= android-native-project-type \"c++\") (insert \"CFLAGS += -mfpu=neon -DHAVE_NEON=1\\n\"))
             ((string= android-native-project-type \"c\") (insert \"CFLAGS += -mfpu=neon -DHAVE_NEON=1\\n\"))))
      ((string= eabi \"armeabi-v7a with VFPV3\")
       (cond ((string= android-native-project-type \"c++\") (insert \"CFLAGS += -mfpu=vfpv3\\n\"))
             ((string= android-native-project-type \"c\")(insert \"CFLAGS += -mfpu=vfpv3\\n\"))))))"
"\"INCLUDE := -I$(ANDROID_NDK_TOOLCHAIN_ROOT)/user/include\" 'n"
 "(when (string= android-native-project-type \"c++\")
(insert \"STL_PATH := $(ANDROID_NDK)/sources/cxx-stl/gnu-libstdc++\\n\")
(insert \"ISYSTEM := -isystem $(STL_PATH)/include -isystem $(STL_PATH)/libs/$(ARMEABI)/include\\n\")
(insert \"CFLAGS += $(ISYSTEM)\\n\")
(insert \"INCLUDE +=  -I$(STL_PATH)/libs/$(ARMEABI)/include -I./\\n\"))"
"(when (file-directory-p (nth 1 (assoc \"ANDROID_NDK\" android-environment-variables)))
(insert \"ANDROID_NDK_SYSROOT := ${ANDROID_NDK}/platforms/android-${ANDROID_API_LEVEL}/arch-arm\\n\")
(cond ((string= android-native-project-type \"c++\")
       (insert \"STL_PATH := ${ANDROID_NDK}/sources/cxx-stl/gnu-libstdc++\\n\")
       (insert \"STL_LIBRARIES_PATH := ${STL_PATH}/libs/${ARMEABI_NDK_NAME}\\n\")
       (insert \"INCLUDE += -I${STL_PATH}/include -I${STL_LIBRARIES_PATH}/include\\n\")
       (insert \"CFLAGS += --sysroot=${ANDROID_NDK_SYSROOT}\\n\"))
      ((string= android-native-project-type \"c\")
       (insert \"CFLAGS += --sysroot=${ANDROID_NDK_SYSROOT}\\n\"))))"
;; "(when (and 
;;         (file-directory-p (nth 1 (assoc \"ANDROID_NDK_TOOLCHAIN_ROOT\" android-environment-variables)))
;;         (string= android-native-project-type \"c++\"))
;; (insert \"STL_LIBRARIES_PATH := ${ANDROID_NDK_TOOLCHAIN_ROOT}/arm-linux-androideabi/lib/${MAKE_SYSTEM_PROCESSOR}\\n\")
;; (unless android-force-arm
;;   (insert \"STL_LIBRARIES_PATH := ${STL_LIBRARIES_PATH}/thumb\\n\")))"
"\"LIB := $(ANDROID_NDK)/platforms/android-$(ANDROID_API_LEVEL)/arch-arm/usr/lib\" 'n"
 "\"ANDROID_NDK_SYSROOT :=${ANDROID_NDK}/platforms/android-${ANDROID_API_LEVEL}/arch-arm\" 'n"
 "\"LIBCRT_BEG := $(LIB)/crtbegin_dynamic.o\" 'n"
 "\"LIBCRT_END := $(LIB)/crtend_android.o\" 'n"
 "\"LDFLAGS := -nostdlib -Wl,--no-undefined -Wl,--entry=main -Wl,--no-undefined -Wl,-z,noexecstack -Bdynamic -Wl,-dynamic-linker,/system/bin/linker -Wl,--fix-cortex-a8 -Wl,-rpath-link=$(LIB) -L$(ANDROID_NDK_TOOLCHAIN_ROOT)/user/libs/$(ARMEABI)\" 'n"
 "\"LDLIBS := -lc -ldl -lm -llog\" 'n"
"(unless android-force-arm
  (insert \"LDFLAGS += -L$(ANDROID_NDK_TOOLCHAIN_ROOT)/lib/gcc/arm-linux-androideabi/4.4.3/$(MAKE_SYSTEM_PROCESSOR)/thumb\\n\"))"
"(when (string= android-native-project-type \"c++\") (insert \"LDFLAGS += -L$(STL_LIBRARIES_PATH) -L$(STL_PATH)/libs/$(ARMEABI) -Wl,--whole-archive -lgcc -lstdc++ -Wl,--no-whole-archive  \\n\"))"
"(when (string= android-native-project-type \"c\") (insert \"LDFLAGS += -Wl,--whole-archive -lgcc -Wl,--no-whole-archive  \\n\"))")
 ;;-disable-multilib
  ""
  :group 'android-mode
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'android-gen-makein-buffer
	    (tempo-define-template
	     "android-makein-template"
	     (android-gen-read-template val)
	     nil
	     "insert makein buffer."))
	  (set-default sym val)))


(defcustom android-makefile-template
  (list
   "\"################################################################################\" 'n"
   "\"#\" 'n"
   "\"#  Makefile for \" android-project-name 'n"
   "\"#\" 'n"
   "\"# Author: \" user-full-name  'n"
   "\"################################################################################\" 'n"
   "\"\" 'n"
   "\"include AndroidMakefile.in\"'n'n"
   "\"CFLAGS += $(DEBUG)\" 'n'n"
   "\"program := \" android-project-name 'n'n"
   "\"INSTALL_DIR := /data/tmp\" 'n 'n"
   "\"ifdef DEVICE\"'n"
   "\"    device_arg := -s $(DEVICE)\"'n"
   "\"else\"'n"
   "\"    device_arg := \"'n"
   "\"endif\"'n'n"   
   "\"all: $(program)\" 'n 'n"
   "\"OBJS += $(program).o\" 'n'n"
   "\"$(program): $(OBJS)\" 'n"
   "\"\\t$(CC) $(INCLUDE) $(CFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)\"'n'n"
   "\"%.o: %.\"(if (string= android-native-project-type \"c++\") (insert \"cpp\") (insert \"c\"))'n"
   "\"\\t$(CC) $(INCLUDE) $(CFLAGS) -c $< -o $@\" 'n'n"
   "\"install: $(program)\"'n"
   "\"\\t$(ANDROID_SDK)/platform-tools/adb $(device_arg) push $(program) $(INSTALL_DIR)/$(program)\"'n"
   "\"\\t$(ANDROID_SDK)/platform-tools/adb $(device_arg) shell chmod 777 $(INSTALL_DIR)/$(program)\" 'n 'n"
   "\"uninstall: $(program)\"'n"
   "\"\\t$(ANDROID_SDK)/platform-tools/adb $(device_arg) shell rm $(INSTALL_DIR)/$(program)\" 'n 'n"
   "\"run:\"'n" "\"\\t$(ANDROID_SDK)/platform-tools/adb $(device_arg) shell $(INSTALL_DIR)/$(program)\" 'n'n"
   "\"debug-install:\"'n"
   "\"\\t$(ANDROID_SDK)/platform-tools/adb $(device_arg) shell chmod 777 $(INSTALL_DIR)/gdbserver\" 'n"
   "\"\\t$(ANDROID_SDK)/platform-tools/adb $(device_arg) push $(PREBUILD)/../gdbserver $(INSTALL_DIR)/gdbserver\"'n'n"
   "\"clean:\"'n"
   "\"\\t@rm -f $(OBJS) $(program)\"'n'n"
   "\".PHONY: check-syntax\"'n"
   "\"check-syntax:\"'n"
   "\"\\t$(CC) $(INCLUDE) $(CFLAGS) -Wall -Wextra -pedantic -fsyntax-only $(CHK_SOURCES)\"'n")
  ""
  :group 'android-mode
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'android-gen-makefile-buffer
	    (tempo-define-template
	     "android-makefile"
	     (android-gen-read-template val)
	     nil
	     "insert makefile buffer."))
	  (set-default sym val)))

(provide 'android-template)
