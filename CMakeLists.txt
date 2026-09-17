cmake_minimum_required(VERSION 3.20)

project(bee LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

file(GLOB_RECURSE SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/bee/*.cpp")
add_executable(bee ${SOURCES})

target_include_directories(bee
	PRIVATE
	${CMAKE_CURRENT_SOURCE_DIR}/
)
