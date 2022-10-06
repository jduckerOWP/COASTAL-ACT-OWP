#!/usr/bin/env bash

####################
### (1) Load all needed environment modules.
module purge
#module load intel/18.0.5.274 impi/2018.0.4
module load szip
module use /home/emc.nemspara/SOFT-hera/modulefiles
module use /scratch2/NCEPDEV/nwprod/hpc-stack/test/hpc-stack/modulefiles/stack
module load hpc
module load hpc-intel
module load hpc-impi
#module load esmf/8_2_0_beta_snapshot_16
module load hdf5_parallel/1.10.6.release
module load netcdf_parallel/4.7.4.release
module load esmf/8_2_0_beta_snapshot_18.1
#module load esmf/8.1.0bs36g


# DFLOW setup for libraries
# external packages
export instdir="/scratch2/COASTAL/coastal/save/COASTAL_ACT_NWC/Libs/intel"
# metis
export METIS_LIB_PATH="/scratch2/COASTAL/coastal/save/Ali.Abdolali/SourceFile/PARMETIS/parmetis-4.0.3"
export METIS_PATH="/scratch2/COASTAL/coastal/save/Ali.Abdolali/SourceFile/PARMETIS/parmetis-4.0.3"    # this key if for NSME models
# petsc
export PETSC_LIB_PATH="$instdir/petsc/lib"

export MODULEPATH="/scratch2/COASTAL/coastal/save/COASTAL_ACT_NWC/Libs/intel/modulefiles:$MODULEPATH"

export METIS_SO_LIB_PATH="$instdir/metis/lib"

export PKG_CONFIG_PATH="$PETSC_LIB_PATH/pkgconfig:$PKG_CONFIG_PATH"
export LD_LIBRARY_PATH="$PETSC_LIB_PATH:$METIS_LIB_PATH::$METIS_SO_LIB_PATH$LD_LIBRARY_PATH"

export LD_LIBRARY_PATH=/scratch2/COASTAL/coastal/save/COASTAL_ACT_NWC/Libs/intel/metis/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$NETCDF_LIBDIR:$LD_LIBRARY_PATH

#NSEM workflow COASTAL App directory pathway
#export NSEMdir="/scratch2/COASTAL/coastal/noscrub/Jason.Ducker/CoastalApp-feature-ww3"

####################
### (2) Set some environments varaiables related to the loaded
###     modules and required to compile the NEMS application properly.

export HDF5HOME=${HDF5}
export NETCDFHOME=${NETCDF}

export NETCDF_CONFIG=${NETCDFHOME:+${NETCDFHOME}/bin/nc-config}
export NETCDF_INCDIR=${NETCDFHOME:+${NETCDFHOME}/include}
export NETCDF_LIBDIR=${NETCDFHOME:+${NETCDFHOME}/lib}

#export ESMFMKFILE=${ESMFMKFILE}
export ESMFMKFILE="$instdir/esmf/lib"
