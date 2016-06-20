

add_executable(publishBin PublishData.cpp)

target_link_libraries(publishBin ${Boost_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
target_link_libraries(publishBin ops)
