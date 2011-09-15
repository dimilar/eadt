macro(default_action)
  # Add these standard paths to the search paths for FIND_LIBRARY
  # to find libraries from these locations first
  if(UNIX)
    set(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH} /lib /usr/lib)
  endif()
  # it _must_ go before PROJECT(OpenCV) in order to work
  if(WIN32)
    set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/install" CACHE PATH "Installation Directory")
  else()
    set(CMAKE_INSTALL_PREFIX "/usr/local" CACHE PATH "Installation Directory")
  endif()

  if(MSVC)
    set(CMAKE_USE_RELATIVE_PATHS ON CACHE INTERNAL "" FORCE)
  endif()
endmacro()

macro(opencv_settings)
  include("${ANDROID_NDK_TOOLCHAIN_ROOT}/user/share/OpenCV/OpenCVConfig.cmake")
  SET (OPENCV_DIR /scratch/programming/android/cmake/opencv/)
  SET (opencv_dependencies opencv_contrib opencv_legacy opencv_objdetect opencv_calib3d opencv_features2d opencv_video opencv_highgui opencv_ml opencv_imgproc opencv_flann opencv_core)

  IF(WITH_ANDROID_CAMERA AND NOT BUILD_SHARED_LIBS)
    LIST(APPEND OPENCV_EXTRA_JAVA_MODULES androidcamera)
  ENDIF()

  if(NOT BUILD_SHARED_LIBS)
    LIST(APPEND opencv_dependencies opencv_androidcamera)
  endif()
  LIST(APPEND opencv_dependencies libjpeg libpng libjasper libtiff zlib)
  foreach(lib ${opencv_dependencies})
    list(APPEND android_dependencies "${lib}")
  endforeach()
endmacro()

macro(find_android_sdk)
  file(TO_CMAKE_PATH "$ENV{ANDROID_SDK}" ANDROID_SDK_ENV_PATH)
  file(TO_CMAKE_PATH "$ENV{ANT_DIR}" ANT_DIR_ENV_PATH)
  file(TO_CMAKE_PATH "$ENV{ProgramFiles}" ProgramFiles_ENV_PATH)
  file(TO_CMAKE_PATH "$ENV{ANDROID_SDK}" ANDROID_SDK_ENV_PATH)

  #find android SDK
  find_host_program(ANDROID_EXECUTABLE
    NAMES android.exe android.bat android
    PATHS "${ANDROID_SDK_ENV_PATH}/tools/"
    "${ProgramFiles_ENV_PATH}/Android/android-sdk/tools/"
    "/opt/android-sdk/tools/"
    "/opt/android-sdk-linux_x86/tools/"
    "/opt/android-sdk-mac_x86/tools/"
    "/opt/android-sdk-linux_86/tools/"
    "/opt/android-sdk-mac_86/tools/"
    )

  find_host_program(ADB_EXECUTABLE
    NAMES  adb.exe adb.bat adb
    PATHS "${ANDROID_SDK_ENV_PATH}/platform-tools/"
    "${ProgramFiles_ENV_PATH}/Android/android-sdk/platform-tools/"
    "/opt/android-sdk/platform-tools/"
    "/opt/android-sdk-linux_x86/platform-tools/"
    "/opt/android-sdk-mac_x86/platform-tools/"
    "/opt/android-sdk-linux_86/platform-tools/"
    "/opt/android-sdk-mac_86/platform-tools/"
    )

  if(ANDROID_EXECUTABLE)
    message(STATUS "    Found android tool: ${ANDROID_EXECUTABLE}")

    get_filename_component(ANDROID_SDK_TOOLS_PATH "${ANDROID_EXECUTABLE}" PATH)

    #read source.properties
    if (EXISTS "${ANDROID_SDK_TOOLS_PATH}/source.properties")
      file(STRINGS "${ANDROID_SDK_TOOLS_PATH}/source.properties" ANDROID_SDK_TOOLS_SOURCE_PROPERTIES_LINES REGEX "^[ ]*[^#].*$")
      foreach(line ${ANDROID_SDK_TOOLS_SOURCE_PROPERTIES_LINES})
        string(REPLACE "\\:" ":" line ${line})
        string(REPLACE "=" ";" line ${line})
        list(GET line 0 line_name)
        list(GET line 1 line_value)
        string(REPLACE "." "_" line_name ${line_name})
        SET(ANDROID_TOOLS_${line_name} "${line_value}")
        MARK_AS_ADVANCED(ANDROID_TOOLS_${line_name})
      endforeach()
    endif()
    if (NOT ANDROID_TOOLS_Pkg_Revision)
      SET(ANDROID_TOOLS_Pkg_Revision "Unknown")
      MARK_AS_ADVANCED(ANDROID_TOOLS_Pkg_Revision)
    endif()
    if (NOT ANDROID_TOOLS_Pkg_Desc)
      SET(ANDROID_TOOLS_Pkg_Desc "Android SDK Tools, revision ${ANDROID_TOOLS_Pkg_Revision}.")
      if (NOT ANDROID_TOOLS_Pkg_Revision GREATER 11)
        SET(ANDROID_TOOLS_Pkg_Desc "${ANDROID_TOOLS_Pkg_Desc} It is recommended to update your SDK tools to revision 12 or newer.")
      endif()
      MARK_AS_ADVANCED(ANDROID_TOOLS_Pkg_Desc)
    endif()
  endif()

  string(REGEX MATCH "[0-9]+$" ANDROID_SDK_TARGET_LEVEL "${ANDROID_SDK_TARGET}")
  #find apache ant
  find_host_program(ANT_EXECUTABLE NAMES ant.bat ant
    PATHS "${ANT_DIR_ENV_PATH}/bin"
    "${ProgramFiles_ENV_PATH}/apache-ant/bin"
    )

  if(ANT_EXECUTABLE)
    execute_process(COMMAND ${ANT_EXECUTABLE} -version
      OUTPUT_VARIABLE ANT_VERSION_FULL
      OUTPUT_STRIP_TRAILING_WHITESPACE)
    string(REGEX MATCH "[0-9]+.[0-9]+.[0-9]+" ANT_VERSION "${ANT_VERSION_FULL}")

    message(STATUS "    Found apache ant ${ANT_VERSION}: ${ANT_EXECUTABLE}")
  endif()

  if (ANDROID_EXECUTABLE AND ANT_EXECUTABLE AND (ANT_VERSION VERSION_GREATER 1.7) AND (ANDROID_SDK_TARGET_LEVEL GREATER 7))
    SET(CAN_BUILD_ANDROID_PROJECTS TRUE)
  else()
    SET(CAN_BUILD_ANDROID_PROJECTS FALSE)
  endif()

  if(CAN_BUILD_ANDROID_PROJECTS)
    SET(BUILD_ANDROID_EXAMPLES TRUE CACHE BOOL "Build examples for Android platform")
  endif()
endmacro()

# creates target "${_target}_android_project" for building standard Android project
macro(add_android_project _target _path _abi)
  if (CAN_BUILD_ANDROID_PROJECTS)
    file(GLOB_RECURSE res_files_all RELATIVE "${_path}" "${_path}/res/*")
    file(GLOB_RECURSE jni_files_all RELATIVE "${_path}" "${_path}/jni/*.c*" "${_path}/jni/*.h*")
    file(GLOB_RECURSE src_files_all RELATIVE "${_path}" "${_path}/src/*.java")

    #remove .svn 
    set(res_files)
    foreach(f ${res_files_all})
      if(NOT f MATCHES "\\.svn")
        list(APPEND res_files "${f}")
      endif()
    endforeach()
    set(jni_files)
    foreach(f ${jni_files_all})
      if(NOT f MATCHES "\\.svn")
        list(APPEND jni_files "${f}")
      endif()
    endforeach()
    set(src_files)
    foreach(f ${src_files_all})
      if(NOT f MATCHES "\\.svn")
        list(APPEND src_files "${f}")
      endif()
    endforeach()

    # get temporary location for the project
    file(RELATIVE_PATH build_path "${CMAKE_CURRENT_SOURCE_DIR}" "${_path}")
    # message("PATH: ${_path}")

    set(build_path "${_path}")
    # message("BUILD_PATH: ${build_path}")
    # copy project to temporary location
    SET(${_target}_project_files)
    foreach(f ${res_files} ${src_files} "AndroidManifest.xml")
      list(APPEND ${_target}_project_files "${build_path}/${f}")
    endforeach()    

    # process default.properties
    # message("BUILD_PATH: ${_path}")
    SET(default-properties-file "${_path}/default.properties")
    if (EXISTS default-properties-file)
      file(STRINGS "${_path}/default.properties" default_properties REGEX "^android\\.library\\.reference\\.1=(.+)$")
    endif()

    if (default_properties)
      STRING(REGEX REPLACE "^android\\.library\\.reference\\.1=(.+)$" "\\1" LIB_1_REFERENCE_PATH "${default_properties}" )

      # has opencv dependency
      # message("OPENCV_REF_PATH:${OPENCV_REFERENCE_PATH}")
      # message("CMAKE_BINARY_DIR dimilar: ${CMAKE_BINARY_DIR}")
      add_custom_command(
        OUTPUT "${build_path}/default.properties"
        OUTPUT "${build_path}/build.xml"
        OUTPUT "${build_path}/local.properties"
        OUTPUT "${build_path}/proguard.cfg"
        COMMAND ${CMAKE_COMMAND} -E echo "" > "default.properties"
        COMMAND ${ANDROID_EXECUTABLE} update project --name "${_target}" --target "${ANDROID_SDK_TARGET}" --library "${LIB_1_REFERENCE_PATH}" --path .
        WORKING_DIRECTORY ${build_path}
        DEPENDS ${${_target}_project_files}
        COMMENT "Updating android project - ${_target}"
        )
    else()
      # has no opencv dependency
      add_custom_command(
        OUTPUT "${build_path}/default.properties"
        OUTPUT "${build_path}/build.xml"
        OUTPUT "${build_path}/local.properties"
        OUTPUT "${build_path}/proguard.cfg"
        COMMAND ${CMAKE_COMMAND} -E echo "" > "default.properties"
        COMMAND ${ANDROID_EXECUTABLE} update project --name "${_target}" --target "${ANDROID_SDK_TARGET}" --path ./
        WORKING_DIRECTORY ${build_path}
        DEPENDS ${${_target}_project_files}
        COMMENT "Updating android project - ${_target}"
        )
    endif()
    if("${build_path}" STREQUAL "${_path}")
      #in case of in-source build default.properties file is not generated (it is just overwritten :)
      SET_SOURCE_FILES_PROPERTIES("${build_path}/default.properties" PROPERTIES GENERATED FALSE)
    endif()
    
    list(APPEND ${_target}_project_files "${build_path}/default.properties" "${build_path}/build.xml" "${build_path}/local.properties" "${build_path}/proguard.cfg")

    # build native part of android project
    if(jni_files)
      INCLUDE_DIRECTORIES("${_path}/jni")
      FILE(STRINGS "${_path}/jni/Android.mk" JNI_LIB_NAME REGEX "LOCAL_MODULE[ ]*:=[ ]*.*" )
      string(REGEX REPLACE "LOCAL_MODULE[ ]*:=[ ]*([a-zA-Z_][a-zA-Z_0-9]*)[ ]*" "\\1" JNI_LIB_NAME "${JNI_LIB_NAME}")

      SET(jni_sources)
      foreach(src ${jni_files})
        list(APPEND jni_sources "${_path}/${src}")
      endforeach()

      add_library(${JNI_LIB_NAME} MODULE ${jni_sources})
      TARGET_LINK_LIBRARIES(${JNI_LIB_NAME} ${LINKER_LIBS} ${android_dependencies})

      set_target_properties(${JNI_LIB_NAME} PROPERTIES
        OUTPUT_NAME "${JNI_LIB_NAME}"
        LIBRARY_OUTPUT_DIRECTORY "${build_path}/libs/${_abi}"
        )
      # message("JNI SOURCES:${JNI_LIB_NAME}")
      add_custom_command(
        TARGET ${JNI_LIB_NAME}
        POST_BUILD
        COMMAND ${CMAKE_STRIP} "${build_path}/libs/${_abi}/*.so"
        )
    endif()

    
    if(JNI_LIB_NAME)
      # list(APPEND ${${_target}_project_files} "${LIBRARY_OUTPUT_PATH}/lib${JNI_LIB_NAME}.so")
      list(INSERT ${${_target}_project_files} 0 "${LIBRARY_OUTPUT_PATH}/lib${JNI_LIB_NAME}.so")
    endif()

    add_custom_command(
      OUTPUT "${build_path}/bin/${_target}-debug.apk"
      OUTPUT "${CMAKE_BINARY_DIR}/bin/${_target}.apk"
      COMMAND ${ANT_EXECUTABLE} -q -noinput -k debug
      COMMAND ${CMAKE_COMMAND} -E copy "${build_path}/bin/${_target}-debug.apk" "${CMAKE_BINARY_DIR}/bin/${_target}.apk"
      WORKING_DIRECTORY ${build_path}
      DEPENDS ${${_target}_project_files}
      COMMENT "Generating bin/${_target}.apk"
      )

    ADD_CUSTOM_TARGET(${_target}_android_project ALL
      DEPENDS "${build_path}/bin/${_target}-debug.apk"
      DEPENDS "${CMAKE_BINARY_DIR}/bin/${_target}.apk"
      )
    if(JNI_LIB_NAME)
      add_dependencies(${_target}_android_project ${JNI_LIB_NAME})
    endif()

  endif()
endmacro()
