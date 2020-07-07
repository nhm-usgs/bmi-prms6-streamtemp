add_library(${bmiprms6streamflow_lib} SHARED
  bmi.f90 bmi_prms_streamflow.f90 m_prms_streamflow.f90)
target_link_libraries(${bmiprms6streamflow_lib} ${EXTRA_LIBS})

install(
  TARGETS ${bmiprms6streamflow_lib}
  EXPORT ${bmiprms6streamflow_lib}-config
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
  INCLUDES DESTINATION include)

install(
  DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/
  DESTINATION include
  FILES_MATCHING PATTERN "*.mod"
  PATTERN "CMakeFiles" EXCLUDE)

install(
  EXPORT ${bmiprms6streamflow_lib}-config
  DESTINATION lib/cmake)
export(
  TARGETS ${bmiprms6streamflow_lib}
  FILE ${bmiprms6streamflow_lib}-config.cmake)
