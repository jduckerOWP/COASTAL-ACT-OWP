###########################################################################
### CMake Build System for the Parametric Hurricane Model (PaHM)
###
### Author: Panagiotis Velissariou <panagiotis.velissariou@noaa.gov>
###
### DEFINITIONS FOR THE "orion" HPC ENVIRONMENT THAT USES THE PGI COMPILERS
###########################################################################

# Check if the pgi module is loaded so we have access to the PGI compilers.
#if("$ENV{PGI}" STREQUAL "")
#  message(FATAL_ERROR "The pgi module is not loaded. Load the module before running this cmake")
#endif()
if("$ENV{PGI}" STREQUAL "")
  message(WARNING "The PGI variable is not set. Is the pgi module loaded?. If not, load the module before running this cmake")
endif()

###========================================
### Check the environment and set required variables
###========================================
include(PlatformFuncs)


########## BEG:: CHECK FOR HDF5 ##########
get_env_hdf5()

if(NOT _DEFINED_HDF5)
  #Do nothing
endif()
########## END:: CHECK FOR HDF5 ##########


########## BEG:: CHECK FOR NETCDF ##########
get_env_netcdf()

if(NOT _DEFINED_NETCDF)
  #message(FATAL_ERROR "Couldn't find any of the NETCDF* environment variables.\n"
  #            "Load the appropriate NetCDF environment module before running cmake.")
endif()
########## END:: CHECK FOR NETCDF ##########


########## BEG:: PLATFORM CUSTOMIZED SETTINGS ##########
#set(_DEFINED_HDF5 TRUE)
#set(_DEFINED_NETCDF TRUE)
########## END:: PLATFORM CUSTOMIZED SETTINGS ##########
