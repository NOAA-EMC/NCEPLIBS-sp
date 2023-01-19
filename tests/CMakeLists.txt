# This is the CMake build file for the test directory of NCEPLIBS-sp.

# Kyle Gerheiser, Ed Hartnett

# This function builds, links, and runs a test program.
function(create_test name kind)
  add_executable(${name}_${kind} ${name}.F90)

  # Include openMP if desired.
  if(OpenMP_Fortran_FOUND)
    target_link_libraries(${name}_${kind} PRIVATE OpenMP::OpenMP_Fortran)
  endif()
  target_link_libraries(${name}_${kind} PRIVATE sp::sp_${kind})
  set_target_properties(${name}_${kind} PROPERTIES COMPILE_FLAGS "${fortran_${kind}_flags}")
  add_test(NAME ${name}_${kind} COMMAND ${name}_${kind})
  target_compile_definitions(${name}_${kind} PUBLIC -DKIND_${kind})  
endfunction()

# At the moment, only _d is tested.
# Build a _4 and _d version of each test and link them to the _4 and
# _d builds of the library, for 4-byte real, and 8-byte real.
foreach(kind ${kinds})
  create_test(test_ncpus ${kind})
  create_test(test_splat ${kind})
  create_test(test_sptezv ${kind})
  create_test(test_fft ${kind})
endforeach()
