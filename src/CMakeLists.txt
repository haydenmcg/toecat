set(SOURCES followerInfo.cpp)

add_executable(followerInfo ${SOURCES})
target_include_directories(
    followerInfo PUBLIC
    ${CMAKE_BINARY_DIR}/include
    ${SOEM_INCLUDE_DIRS}
    )

target_link_libraries(followerInfo 
    PUBLIC soem)