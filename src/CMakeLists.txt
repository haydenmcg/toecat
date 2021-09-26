add_executable(followerInfo followerInfo.cpp)
target_include_directories(
    followerInfo PUBLIC
    ${CMAKE_BINARY_DIR}/include
    ${SOEM_INCLUDE_DIRS}
    )

target_link_libraries(followerInfo 
    PUBLIC soem)




#add_library(tcb-lib STATIC
#    main.cpp
#    )
#
#message(STATUS "SOEM INCLUDE DIRS: ${SOEM_INCLUDE_DIRS}")
#
#target_include_directories(
#    tcb-lib PUBLIC
#    ${CMAKE_BINARY_DIR}/include
#    ${SOEM_INCLUDE_DIRS}
#    )
#
#find_package(Threads REQUIRED)
#target_link_libraries(tcb-lib
#    PUBLIC soem
#    PRIVATE Threads::Threads
#    PRIVATE rt
#    )


