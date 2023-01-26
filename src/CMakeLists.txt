# This is the CMake file for the src directory of NCEPLIBS-sp.
#
# Mark Potts, Kyle Gerheiser

# These are the source files.
set(fortran_src fftpack.F lapack_gen.F ncpus.F spanaly.f spdz2uv.f
speps.f spfft1.f spffte.f spfft.f spfftpt.f spgradq.f spgradx.f
spgrady.f splaplac.f splat.F splegend.f sppad.f spsynth.f sptezd.f
sptez.f sptezmd.f sptezm.f sptezmv.f sptezv.f sptgpmd.f sptgpm.f
sptgpmv.f sptgpsd.f sptgps.f sptgpsv.f sptgptd.f sptgpt.f sptgptsd.f
sptgptvd.f sptgptv.f sptrand.f sptran.f sptranf0.f sptranf1.f
sptranf.f sptranfv.f sptranv.f sptrund.f sptrun.f sptrung.f sptrungv.f
sptrunl.f sptrunm.f sptrunmv.f sptruns.f sptrunsv.f sptrunv.f
spuv2dz.f spvar.f spwget.f )

# We can build two versions of the library, _4 for standard, and _d for
# 8-byte float.
foreach(kind ${kinds})
  set(lib_name ${PROJECT_NAME}_${kind})

  # Create object library. The object library will be used to build
  # static and shared libraries.
  add_library(${lib_name}_objlib OBJECT ${fortran_src})
  if(BUILD_SHARED_LIBS)
    set_property(TARGET ${lib_name}_objlib PROPERTY POSITION_INDEPENDENT_CODE 1)
  endif()
  add_library(${PROJECT_NAME}::${lib_name} ALIAS ${lib_name}_objlib)

  if(BUILD_STATIC_LIBS)
    # Create static object library from object library.
    add_library(${lib_name}_static STATIC $<TARGET_OBJECTS:${lib_name}_objlib>)
    set_target_properties(${lib_name}_static PROPERTIES OUTPUT_NAME ${lib_name})
    set_target_properties(${lib_name}_static PROPERTIES EXPORT_NAME ${lib_name})
    set_target_properties(${lib_name}_static PROPERTIES EXPORT_NAME ${lib_name}_static)
    list(APPEND LIB_TARGETS ${lib_name}_static)
  endif()

  if(BUILD_SHARED_LIBS)
    # Create shared object library from object library.
    add_library(${lib_name}_shared SHARED $<TARGET_OBJECTS:${lib_name}_objlib>)
    set_target_properties(${lib_name}_shared PROPERTIES OUTPUT_NAME ${lib_name})
    set_target_properties(${lib_name}_shared PROPERTIES EXPORT_NAME ${lib_name})
    set_target_properties(${lib_name}_shared PROPERTIES SOVERSION 0)
    list(APPEND LIB_TARGETS ${lib_name}_shared)
  endif()

  # Set the flags for _4 or _d.
  set(BUILD_FLAGS "${fortran_${kind}_flags}")

  # Deal with OpenMP.
  if(OpenMP_Fortran_FOUND)
    if(BUILD_SHARED_LIBS)
      target_link_libraries(${lib_name}_shared PRIVATE OpenMP::OpenMP_Fortran)
    endif()
    if(BUILD_STATIC_LIBS)
      target_link_libraries(${lib_name}_static PRIVATE OpenMP::OpenMP_Fortran)
    endif()
  endif()
  
  set_target_properties(${lib_name}_objlib PROPERTIES COMPILE_FLAGS "${BUILD_FLAGS}")

endforeach()

install(
  TARGETS ${LIB_TARGETS}
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

### Package config
include(CMakePackageConfigHelpers)
set(CONFIG_INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

export(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake)

configure_package_config_file(
  ${CMAKE_SOURCE_DIR}/cmake/PackageConfig.cmake.in ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
  INSTALL_DESTINATION ${CONFIG_INSTALL_DESTINATION})
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

write_basic_package_version_file(
  ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion)
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

install(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})
