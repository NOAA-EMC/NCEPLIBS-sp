# This is the main CMake file for NCEPLIBS-sp.
#
# Mark Potts, Kyle Gerheiser
cmake_minimum_required(VERSION 3.15)

file(STRINGS "VERSION" pVersion)

project(sp VERSION ${pVersion} LANGUAGES Fortran)

option(OPENMP "use OpenMP threading" OFF)
option(ENABLE_TESTS "Enable tests" OFF)

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
    "Release"
    CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
    "MinSizeRel" "RelWithDebInfo")
endif()

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(
    WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

add_subdirectory(src)

if (${ENABLE_TESTS})
  find_package(PFUNIT REQUIRED)
  enable_testing()
  add_subdirectory(tests)
endif()

# Determine whether or not to generate documentation.
OPTION(ENABLE_DOCS "Enable generation of doxygen-based documentation." OFF)
IF(ENABLE_DOCS)
  FIND_PACKAGE(Doxygen REQUIRED)
ENDIF()
ADD_SUBDIRECTORY(docs)  



