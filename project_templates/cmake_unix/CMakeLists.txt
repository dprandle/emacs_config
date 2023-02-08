cmake_minimum_required(VERSION 3.11.0)

project(PROJ_NAME)

# These have the option to be different since you might want a version attached to the target name but not the app.
# There may be more than a single app/target per project - so we by default set them to the PROJ_NAME we can easily change that later.
set(TARGET_NAME PROJ_NAME)
set(APP_NAME PROJ_NAME)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)

set(PLIST_FILE ${CMAKE_SOURCE_DIR}/cmake/info.plist)
set(SRC_DIR ${CMAKE_SOURCE_DIR}/src)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# Set the src files for the project
file(GLOB SRC_FILES
  "${SRC_DIR}/*.cpp")

if(WIN32)
  add_executable(${TARGET_NAME} ${SRC_FILES})
elseif(APPLE)
  add_executable(${TARGET_NAME} MACOSX_BUNDLE ${SRC_FILES})
  set_target_properties(${TARGET_NAME} PROPERTIES MACOSX_BUNDLE_INFO_PLIST ${PLIST_FILE})
  set_target_properties(${TARGET_NAME} PROPERTIES MACOSX_BUNDLE_BUNDLE_NAME "${APP_NAME}")
  set_target_properties(${TARGET_NAME} PROPERTIES MACOSX_BUNDLE_BUNDLE_GUI_IDENTIFIER "com.${APP_NAME}")
else()
  add_executable(${TARGET_NAME} ${SRC_FILES})
endif()

# target_link_libraries(${TARGET_NAME} lib1 lib2 ... libn)
