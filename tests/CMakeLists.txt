# This is the CMake build file for the test directory of NCEPLIBS-sp.

# Kyle Gerheiser, Ed Hartnett

link_libraries(sp::sp_d)

function(make_test test_name file)
  add_executable(${test_name} ${file})
  add_test(${test_name} ${test_name})
endfunction()

make_test(test_ncpus test_ncpus.F90)
make_test(test_splat test_splat.f90)
make_test(test_sptezv test_sptezv.f90)
make_test(test_fft test_fft.f90)




