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
OPTION(ENABLE_DOCS "Enable generation of doxygen-based documentation" OFF)
option(OPENMP "use OpenMP threading" OFF)
option(BUILD_SHARED_LIBS "Build shared libraries" OFF)
option(BUILD_4 "Build the 4-byte real version of the library, libsp_4.a" ON)
option(BUILD_D "Build the 8-byte real version of the library, libsp_d.a" ON)
option(BUILD_8 "Build the 8-byte integer version of the library, libsp_8.a" OFF)

# Figure whether user wants a _4, a _d, or both libraries.
if(BUILD_4)
  set(kinds "4")
endif()
if(BUILD_D)
  set(kinds ${kinds} "d")
endif()
if(BUILD_8)
  set(kinds ${kinds} "8")
endif()
if(NOT BUILD_4 AND NOT BUILD_D AND NOT BUILD_8)
  message(FATAL_ERROR "At least one of BUILD_4 or BUILD_D must be turned on")
endif()
message(STATUS "Library kinds that will be build: ${kinds}")

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

# Set compiler flags.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS
    "-g -traceback -auto -convert big_endian -assume byterecl -fp-model strict -fpp ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(fortran_d_flags "-i4 -r8")
  set(fortran_8_flags "-i8 -r8")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "-g -fbacktrace -fconvert=big-endian ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(fortran_d_flags "-fdefault-real-8")
  set(fortran_8_flags "-fdefault-integer-8 -fdefault-real-8")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(NVHPC)$")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(fortran_d_flags "-i4 -r8")
  set(fortran_8_flags "-i8 -r8")
endif()

# Handle argument mismatch problems for GNU versions > 10.
if(${CMAKE_Fortran_COMPILER_ID} MATCHES "^(GNU)$" AND ${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w -fallow-argument-mismatch -fallow-invalid-boz")
endif()

# This is where the library code is.
add_subdirectory(src)

# Build and run tests.
include(CTest)
if(BUILD_TESTING)
    add_subdirectory(tests)
endif()

# Does the user want to generate documentation?
if(ENABLE_DOCS)
  find_package(Doxygen REQUIRED)
endif()
add_subdirectory(docs)  



