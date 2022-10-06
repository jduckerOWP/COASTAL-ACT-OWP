#!/bin/bash

# Ducker - 4/27/2022 - this script is the driver for building D-Flow and loading
# library prerequistes for the latest D-FlowFM version 2022.03 GA repository.
# D-Flow now uses CMake capabilitest to build and compile its internal source,
# while building the necessary libraries to run the D-Flow executable within NEMS.


#://oss.deltares.nl/web/delft3d/source-code#prerequisites
# IMPORTANT: Use the same vendor (Intel or GNU) for both the C++ and the Fortran compiler.

# Build source code using build.sh function. Libraries and configurations are already
# predefined within setenv_no_modules.sh script in DFlow/src directory.
source dflow-nuopc-env.sh

echo "Building D-Flow source code using CMake build.sh script...."
#./../build.sh
echo "Finished making dflowfm...."

echo "Moving CMake built D-Flow libraries to D-Flow cap directory...."
cd $DFLOW_BUILDLIBS
#cp -R ../**/*.a ./lib
#cp -R ../**/*.so ./lib
#cp -R ../**/*.a ./
#cp -R ../**/*.so ./


#scp -r * $DFLOW_CAPDIR
scp -r ./bin $DFLOW_CAPDIR
scp -r ./share $DFLOW_CAPDIR
mkdir $DFLOW_CAPDIR/lib
#scp -r ./lib/libpetsc* $DFLOW_CAPDIR/lib
#scp -r ./lib/libhdf* $DFLOW_CAPDIR/lib
#scp -r ./lib/libnetcdf* $DFLOW_CAPDIR/lib
#scp -r ./lib/libgdal* $DFLOW_CAPDIR/lib
scp -r ./lib/libdflowfm.so $DFLOW_CAPDIR/lib
scp -r ./lib/libdflowfm.so $DFLOW_CAPDIR
echo "Configuring D-Flow libraries to couple with NEMS...."
cd $DFLOW_CAPDIR
chmod +x lib/*
#ranlib $DFLOW_CAPDIR/lib/*.a
