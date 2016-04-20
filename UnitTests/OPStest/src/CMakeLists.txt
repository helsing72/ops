

add_executable(publishBin PublishData.cpp)

target_link_libraries(publishBin ${Boost_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
# the publishData don't use gtest (?)
#target_link_libraries(publishBin ${GTEST_LIBRARIES})
target_link_libraries(publishBin ops)
