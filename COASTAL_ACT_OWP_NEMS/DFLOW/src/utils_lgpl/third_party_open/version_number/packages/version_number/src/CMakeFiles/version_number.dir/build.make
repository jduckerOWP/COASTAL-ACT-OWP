# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.20

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /apps/cmake/3.20.1/bin/cmake

# The command to remove a file.
RM = /apps/cmake/3.20.1/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /scratch2/COASTAL/coastal/noscrub/Jason.Ducker/DFlowFM_2022.03/src/third_party_open/version_number/packages/version_number

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /scratch2/COASTAL/coastal/noscrub/Jason.Ducker/DFlowFM_2022.03/src/third_party_open/version_number/packages/version_number/src

# Include any dependencies generated for this target.
include CMakeFiles/version_number.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include CMakeFiles/version_number.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/version_number.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/version_number.dir/flags.make

CMakeFiles/version_number.dir/version_number.f90.o: CMakeFiles/version_number.dir/flags.make
CMakeFiles/version_number.dir/version_number.f90.o: version_number.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/scratch2/COASTAL/coastal/noscrub/Jason.Ducker/DFlowFM_2022.03/src/third_party_open/version_number/packages/version_number/src/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object CMakeFiles/version_number.dir/version_number.f90.o"
	/apps/intel/compilers_and_libraries_2018/linux/mpi/intel64/bin/mpiifort $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /scratch2/COASTAL/coastal/noscrub/Jason.Ducker/DFlowFM_2022.03/src/third_party_open/version_number/packages/version_number/src/version_number.f90 -o CMakeFiles/version_number.dir/version_number.f90.o

CMakeFiles/version_number.dir/version_number.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/version_number.dir/version_number.f90.i"
	/apps/intel/compilers_and_libraries_2018/linux/mpi/intel64/bin/mpiifort $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /scratch2/COASTAL/coastal/noscrub/Jason.Ducker/DFlowFM_2022.03/src/third_party_open/version_number/packages/version_number/src/version_number.f90 > CMakeFiles/version_number.dir/version_number.f90.i

CMakeFiles/version_number.dir/version_number.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/version_number.dir/version_number.f90.s"
	/apps/intel/compilers_and_libraries_2018/linux/mpi/intel64/bin/mpiifort $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /scratch2/COASTAL/coastal/noscrub/Jason.Ducker/DFlowFM_2022.03/src/third_party_open/version_number/packages/version_number/src/version_number.f90 -o CMakeFiles/version_number.dir/version_number.f90.s

# Object files for target version_number
version_number_OBJECTS = \
"CMakeFiles/version_number.dir/version_number.f90.o"

# External object files for target version_number
version_number_EXTERNAL_OBJECTS =

version_number: CMakeFiles/version_number.dir/version_number.f90.o
version_number: CMakeFiles/version_number.dir/build.make
version_number: CMakeFiles/version_number.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/scratch2/COASTAL/coastal/noscrub/Jason.Ducker/DFlowFM_2022.03/src/third_party_open/version_number/packages/version_number/src/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking Fortran executable version_number"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/version_number.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/version_number.dir/build: version_number
.PHONY : CMakeFiles/version_number.dir/build

CMakeFiles/version_number.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/version_number.dir/cmake_clean.cmake
.PHONY : CMakeFiles/version_number.dir/clean

CMakeFiles/version_number.dir/depend:
	cd /scratch2/COASTAL/coastal/noscrub/Jason.Ducker/DFlowFM_2022.03/src/third_party_open/version_number/packages/version_number/src && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /scratch2/COASTAL/coastal/noscrub/Jason.Ducker/DFlowFM_2022.03/src/third_party_open/version_number/packages/version_number /scratch2/COASTAL/coastal/noscrub/Jason.Ducker/DFlowFM_2022.03/src/third_party_open/version_number/packages/version_number /scratch2/COASTAL/coastal/noscrub/Jason.Ducker/DFlowFM_2022.03/src/third_party_open/version_number/packages/version_number/src /scratch2/COASTAL/coastal/noscrub/Jason.Ducker/DFlowFM_2022.03/src/third_party_open/version_number/packages/version_number/src /scratch2/COASTAL/coastal/noscrub/Jason.Ducker/DFlowFM_2022.03/src/third_party_open/version_number/packages/version_number/src/CMakeFiles/version_number.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/version_number.dir/depend
