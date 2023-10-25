# This is the CMake build file for the test directory of NCEPLIBS-sp.

# Kyle Gerheiser, Ed Hartnett, Alex Richert

execute_process(COMMAND cmake -E create_symlink
  "${CMAKE_CURRENT_SOURCE_DIR}/data"
  "${CMAKE_CURRENT_BINARY_DIR}/data"
  )

# This function builds, links, and runs a test program.
function(create_test name kind timeout)
  add_executable(${name}_${kind} ${name}.F90)

  # Include openMP if desired.
  if(OpenMP_Fortran_FOUND)
    target_link_libraries(${name}_${kind} PRIVATE OpenMP::OpenMP_Fortran)
    execute_process(COMMAND awk "BEGIN {print ${timeout}/2}" OUTPUT_VARIABLE timeout)
  endif()
  target_link_libraries(${name}_${kind} PRIVATE sp::sp_${kind})
  set_target_properties(${name}_${kind} PROPERTIES COMPILE_FLAGS "${fortran_${kind}_flags}")
  add_test(NAME ${name}_${kind} COMMAND ${name}_${kind})
  target_compile_definitions(${name}_${kind} PUBLIC -DKIND_${kind})
  set_tests_properties(${name}_${kind} PROPERTIES TIMEOUT ${timeout})
endfunction()

# At the moment, only _d is tested.
# Build a _4 and _d version of each test and link them to the _4 and
# _d builds of the library, for 4-byte real, and 8-byte real.
foreach(kind ${kinds})
  create_test(test_ncpus ${kind} 0.05)
  create_test(test_splat ${kind} 0.05)
  create_test(test_sptezv ${kind} 0.2)
  create_test(test_fft ${kind} 0.05)
  create_test(test_sptrung ${kind} 0.05)
  create_test(test_sptrungv ${kind} 0.1)
endforeach()
