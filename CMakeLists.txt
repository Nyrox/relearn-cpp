# CMakeList.txt : CMake project for RelearningCpp, include source and define
# project specific logic here.
#
cmake_minimum_required (VERSION 3.8)

project ("RelearningCpp")

add_compile_options(/permissive- /std:c++latest)
# Add source to this project's executable.
add_executable (RelearningCpp "RelearningCpp.cpp" "RelearningCpp.h" "gl.h" "gl.c")




set(GLFW_BUILD_DOCS OFF CACHE BOOL "" FORCE)
set(GLFW_BUILD_TESTS OFF CACHE BOOL "" FORCE)
set(GLFW_BUILD_EXAMPLES OFF CACHE BOOL "" FORCE)

add_subdirectory(glfw)
target_link_libraries(RelearningCpp glfw)
target_link_libraries(RelearningCpp opengl32)





# TODO: Add tests and install targets if needed.
