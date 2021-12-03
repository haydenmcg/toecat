set(SOURCES 
        createFollowerStruct.cpp
        ecat2String.cpp)

add_executable(createFollowerStruct ${SOURCES})

target_include_directories(
    createFollowerStruct PUBLIC
    ${CMAKE_SOURCE_DIR}/app/createFollowerStruct
    ${CMAKE_BINARY_DIR}/include
    ${SOEM_INCLUDE_DIRS}
    )

target_link_libraries(createFollowerStruct 
    PUBLIC soem)