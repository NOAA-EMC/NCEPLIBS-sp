# This is the CMake build file for the test directory of NCEPLIBS-sp.

# Kyle Gerheiser, Ed Hartnett

link_libraries(sp::sp_d)

function(make_test test_name)
  add_executable(${test_name} ${test_name}.f90)
  add_test(${test_name} ${test_name})
endfunction()

make_test(test_ncpus)
make_test(test_splat)
make_test(test_sptezv)
make_test(test_fft)


