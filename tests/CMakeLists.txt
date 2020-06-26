set(test_srcs test_sp.pf)

add_pfunit_ctest (sp_mod_test
  TEST_SOURCES test_sp_mod.pf
  LINK_LIBRARIES sp::sp_4
)
