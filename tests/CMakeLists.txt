# This is the CMake build file for the test directory of NCEPLIBS-sp.

# Kyle Gerheiser, Ed Hartnett

# At the moment, only _d is tested.
if(BUILD_D)

  # Link to the sp library.
  link_libraries(sp::sp_d)

  # Include openMP if desired.
  if(OpenMP_Fortran_FOUND)
    link_libraries(OpenMP::OpenMP_Fortran)
  endif()
  
  # This function creates and runs one test.
  function(make_test test_name file)
    add_executable(${test_name} ${file})
    add_test(${test_name} ${test_name})
  endfunction()
  
  make_test(test_ncpus test_ncpus.F90)
  make_test(test_splat test_splat.f90)
  make_test(test_sptezv test_sptezv.f90)
  make_test(test_fft test_fft.f90)
endif()
