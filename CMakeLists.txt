cmake_minimum_required(VERSION 3.20)

project(bee LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_executable(bee
	bee/main.cpp
	bee/lexer/Token.cpp
)

target_include_directories(bee
	PRIVATE
	${CMAKE_CURRENT_SOURCE_DIR}/
)
