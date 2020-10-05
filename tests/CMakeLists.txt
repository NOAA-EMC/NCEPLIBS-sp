# This is the CMake build file for the test directory of NCEPLIBS-sp.

# Kyle Gerheiser, Ed Hartnett

add_executable (tst_sp tst_sp.F)
add_test(tst_sp tst_sp)
target_link_libraries (tst_sp sp_d)

