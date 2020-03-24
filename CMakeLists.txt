cmake_minimum_required(VERSION 3.15)

file(STRINGS "VERSION" pVersion)

project(sp VERSION ${pVersion} LANGUAGES Fortran)

option(OPENMP "use OpenMP threading" OFF)

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                                               "MinSizeRel" "RelWithDebInfo")
endif()

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU|Clang|AppleClang)$")
  message(
    WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS
      "-g -traceback -auto -convert big_endian -assume byterecl -fp-model strict -fpp"
  )
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(fortran_d_flags "-i4 -r8")
  set(fortran_8_flags "-i8 -r8")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(CMAKE_Fortran_FLAGS "-g -fbacktrace -fconvert=big-endian")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(fortran_d_flags "-fdefault-real-8")
  set(fortran_8_flags "-fdefault-integer-8 -fdefault-real-8")
endif()

find_package(FFTW3 CONFIG REQUIRED)

if(APPLE)
  add_compile_definitions(APPLE)
elseif(UNIX)
  add_compile_definitions(LINUX)
endif()

if(OPENMP)
  find_package(OpenMP REQUIRED COMPONENTS Fortran)
endif()
if(OpenMP_FOUND)
  add_compile_definitions(OPENMP)
endif()

add_subdirectory(src)

set(kinds "d")
foreach(kind ${kinds})
  set(lib_name ${PROJECT_NAME}_${kind})
  set(versioned_lib_name ${PROJECT_NAME}_v${PROJECT_VERSION}_${kind})
  set(module_dir ${CMAKE_CURRENT_BINARY_DIR}/include_${kind})

  set(BUILD_FLAGS "${fortran_${kind}_flags}")
  if(OpenMP_Fortran_FOUND)
    set(BUILD_FLAGS "${BUILD_FLAGS} ${OpenMP_Fortran_FLAGS}")
  endif()

  add_library(${lib_name} STATIC ${fortran_src})
  set_target_properties(${lib_name} PROPERTIES OUTPUT_NAME
                                               ${versioned_lib_name})
  set_target_properties(${lib_name} PROPERTIES COMPILE_FLAGS
                                               "${BUILD_FLAGS}")
  set_target_properties(${lib_name} PROPERTIES Fortran_MODULE_DIRECTORY
                                               ${module_dir})
  set_target_properties(${lib_name} PROPERTIES INTERFACE_LINK_LIBRARIES
                                               ${lib_name})
  target_include_directories(
    ${lib_name}
    PUBLIC $<BUILD_INTERFACE:${module_dir}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_PREFIX}/include_${kind}>)

  target_link_libraries(${lib_name} FFTW3::fftw3)

  install(
    TARGETS ${lib_name}
    EXPORT ${PROJECT_NAME}-config
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib)
  install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX})
endforeach()

install(EXPORT ${PROJECT_NAME}-config
        DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/cmake/${PROJECT_NAME})
