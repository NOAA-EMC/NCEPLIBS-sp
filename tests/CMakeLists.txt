# This is the CMake build file for the test directory of NCEPLIBS-sp.

# Kyle Gerheiser, Ed Hartnett

link_libraries(sp::sp_d)

add_executable(test_ncpus test_ncpus.F90)
add_executable(test_splat test_splat.f90)
add_executable(test_sptezv test_sptezv.f90)

add_test(test_ncpus test_ncpus)
add_test(test_splat test_splat)
add_test(test_sptezv test_sptezv)


