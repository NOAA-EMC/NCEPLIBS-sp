# This is the main CMake file for NCEPLIBS-sp.
#
# Mark Potts, Kyle Gerheiser, Ed Hartnett
cmake_minimum_required(VERSION 3.15)

# Get the version from the VERSION file.
file(STRINGS "VERSION" pVersion)
project(sp VERSION ${pVersion} LANGUAGES Fortran)

# Set the version for the documentation.
SET(PACKAGE_VERSION ${pVersion})

include(GNUInstallDirs)

# Handle user options.
option(OPENMP "use OpenMP threading" OFF)

# Check build type.
if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
    "Release"
    CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
    "MinSizeRel" "RelWithDebInfo")
endif()

# If the user selected openmp, find it.
if(OPENMP)
  find_package(OpenMP REQUIRED COMPONENTS Fortran)
endif()

# If we have openmp, add its compile definitions.
if(OpenMP_FOUND)
  add_compile_definitions(OPENMP)
endif()

# This is where the library code is.
add_subdirectory(src)

# Build and run tests.
include(CTest)
if (BUILD_TESTING)
    add_subdirectory(tests)
endif()

# Create distributions.
if (UNIX)
  set(CPACK_PACKAGE_CONTACT "NOAA EMC")
  set(CPACK_GENERATOR "STGZ;TGZ;TZ;DEB;RPM")
  include(CPack)
endif()
  
# Determine whether or not to generate documentation.
OPTION(ENABLE_DOCS "Enable generation of doxygen-based documentation." OFF)
IF(ENABLE_DOCS)
  FIND_PACKAGE(Doxygen REQUIRED)
ENDIF()
ADD_SUBDIRECTORY(docs)  



