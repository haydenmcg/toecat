cmake_minimum_required(VERSION 3.14)
project(tcbTest
    DESCRIPTION "For EtherCAT Fun (FEF)"
    VERSION 0.0.1
    LANGUAGES C CXX
    )

set(CMAKE_BUILD_TYPE Debug)

add_compile_options(
    -Wall
    -Wextra
    -Werror
    -Wno-missing-field-initializers
    )

if(DISABLE_DEBUG_PRINTS)
    remove_definitions(-DDEBUG)
else(DISABLE_DEBUG_PRINTS)
    add_definitions(-DDEBUG)
endif(DISABLE_DEBUG_PRINTS)


# for pretty pretty printing
string(ASCII 27 Esc)
set(ColorReset "${Esc}[m")
set(Yellow "${Esc}[33m")
set(Green "${Esc}[32m")

# install project headers to binary directory
file(GLOB_RECURSE includes RELATIVE
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${CMAKE_CURRENT_SOURCE_DIR}/src/*.h
    ${CMAKE_CURRENT_SOURCE_DIR}/src/*.hh
    ${CMAKE_CURRENT_SOURCE_DIR}/src/*.hpp
    )
foreach(include ${includes})
    configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/src/${include}"
        "${CMAKE_BINARY_DIR}/include/${PROJECT_NAME}/${include}"
        COPYONLY
        )
endforeach()

####### Dependencies ########

include(FetchContent)
FetchContent_Declare(soem
    GIT_REPOSITORY https://github.com/OpenEtherCATsociety/SOEM.git
    GIT_TAG 354466a534d2fcd60c670b1b9e94b38035d503bc
    )
FetchContent_MakeAvailable(soem)

set(SOEM_INCLUDE_DIRS
    ${soem_SOURCE_DIR}/soem
    ${soem_SOURCE_DIR}/osal
    )
if(WIN32)
    set(OS "win32")
elseif(UNIX AND NOT APPLE)
    set(OS "linux")
elseif(APPLE)
    set(OS "macosx")
elseif(${CMAKE_SYSTEM_NAME} MATCHES "rt-kernel")
    set(OS "rtk")
    list(APPEND SOEM_INCLUDE_DIRS ${soem_SOURCE_DIR}/oshw/${OS}/${ARCH})
elseif(${CMAKE_SYSTEM_NAME} MATCHES "rtems")
    set(OS "rtems")
endif()

list(APPEND SOEM_INCLUDE_DIRS
    ${soem_SOURCE_DIR}/osal/${OS}
    ${soem_SOURCE_DIR}/oshw/${OS}
    )

add_subdirectory(src)

######### Install Rules #########
if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
    #install(DIRECTORY tcbTest DESTINATION bin)
    install(DIRECTORY ${CMAKE_BINARY_DIR}/include/tcb DESTINATION include)
    install(DIRECTORY ${CMAKE_BINARY_DIR}/bin/ DESTINATION bin)
    install(TARGETS followerInfo DESTINATION bin)
    #install(TARGETS followerinfo DESTINATION bin)
endif()

#install(TARGETS tcbTest DESTINATION bin)
