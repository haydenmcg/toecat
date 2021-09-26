set(SOURCES createFollowerStruct.cpp)

add_executable(createFollowerStruct ${SOURCES})
target_include_directories(
    createFollowerStruct PUBLIC
    ${CMAKE_BINARY_DIR}/include
    ${SOEM_INCLUDE_DIRS}
    )

target_link_libraries(createFollowerStruct 
    PUBLIC soem)