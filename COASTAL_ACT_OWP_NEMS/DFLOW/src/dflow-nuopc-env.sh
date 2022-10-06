#!/usr/bin/env bash

module load intel/18.0.5.274
module load szip/2.1
module load impi/2018.0.4

# proj:
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/scratch2/COASTAL/coastal/save/COASTAL_ACT_NWC/Libs/intel/proj/lib64/pkgconfig

#netcdf-fortran
#export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/scratch1/NCEPDEV/nems/emc.nemspara/soft/netcdf_parallel/lib/pkgconfig
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/scratch1/NCEPDEV/nems/emc.nemspara/soft/netcdf_parallel_release/lib/pkgconfig

# gdal:
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/scratch2/COASTAL/coastal/save/COASTAL_ACT_NWC/Libs/intel/gdal/lib64/pkgconfig

export CC=mpiicc
export CXX=mpiicpc
export FC=mpiifort
export F77=mpiifort

# If a copy of this script is in NEMS/modulefiles/hera, the currdir
# sets to nems location and messup the real path.
currdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
export DFLOW_CAPDIR="$currdir/engines_gpl/dflowfm/packages/nwm_hydraulic_nuopc"

export DFLOW_BUILDLIBS="/scratch2/COASTAL/coastal/noscrub/Jason.Ducker/DFlowFM_2022.03/build_dflowfm/install"

# where to install external packages
export instdir="/scratch2/COASTAL/coastal/save/COASTAL_ACT_NWC/Libs/intel"
# metis
export METIS_LIB_PATH="$instdir/metis/lib"
export METIS_PATH="$instdir/metis/lib"    # this key if for NSME models
# petsc
export PETSC_LIB_PATH="$instdir/petsc/lib"

export GDAL_LIB_PATH="/scratch2/COASTAL/coastal/save/COASTAL_ACT_NWC/Libs/intel/gdal/lib64"
export PROJ_LIB_PATH="/scratch2/COASTAL/coastal/save/COASTAL_ACT_NWC/Libs/intel/proj/lib64"

#export NETCDF_LIB_PATH="/scratch1/NCEPDEV/nems/emc.nemspara/soft/netcdf_parallel/lib"
export NETCDF_LIB_PATH="/scratch1/NCEPDEV/nems/emc.nemspara/soft/netcdf_parallel_release/lib"

export DFLOW_LIB_PATH="$DFLOW_CAPDIR/lib"

export PKG_CONFIG_PATH="$PETSC_LIB_PATH/pkgconfig:$PKG_CONFIG_PATH"
export LD_LIBRARY_PATH="$DFLOW_LIB_PATH:$LD_LIBRARY_PATH"
export PATH="$DFLOW_CAPDIR/bin:$PATH"

