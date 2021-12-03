
set(SOURCES eepromtool.cpp)

add_executable(eepromtool ${SOURCES})

target_include_directories(
    eepromtool PUBLIC
    ${CMAKE_BINARY_DIR}/include
    ${SOEM_INCLUDE_DIRS}
    )

target_link_libraries(eepromtool 
    PUBLIC soem)

