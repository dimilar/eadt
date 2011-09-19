# Installation
put the two lines below in your ```.emacs```:

     (add-to-list 'load-path "/path/to/eadt")
     (require 'android)

Ensure the following variables are configured:
1. Type of floating point support, default value is "armeabi-v7a with NEON"
   (customize-variable 'android-armeabi)  

2. The pathes where to find the tools in common use
    (customize-variable 'android-environment-variables) 
Optionally, if you plan to use the cmake as the build tools, please configure the two variables below

(setq android-cmake-include-file "/path/to/the/android.cmake")
(setq android-cmake-toolchain-file "/path/to/the/android.toolchain.cmake")
usually, these two files is in the same folder as the lisp code.

3. If you have known the name of your device/emulator, please configure the variable:
 (customize-variable 'android-device)   
or (set-default android-device "the name of your device/emulator")

# Default key bindings
 - Default key bindings are:
   - C-c C-c b Compile the source code of a given android project
   - C-c C-c i Install the binary program or apk to your device/emulator
   - C-c C-c r Run the program or package on the device/emulator
   - C-c C-c c Get rid of all the objects files, executables or apk
   - C-c C-c u Remove the binary program or apk from your device/emulator
   - C-c C-c d a Call the jdb to debug the Android activity 
   - C-c C-c d j Remotely debug the Java Native Interface with gdb
   - C-c C-c d n Debug the binary program of the native project
   - C-c C-c l ReLaunch the Android logging system
   - C-c C-c e Start the emulator with some Android Virtual Device
   - C-c C-c D Start the debugging tool DDMS (the Dalvik Debug Monitor Server)

# Features
