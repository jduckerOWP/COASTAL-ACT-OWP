@ echo off
title run_dflowfm
    rem
    rem This script runs dfmoutput on Windows
    rem Adapt and use it for your own purpose
    rem
    rem Usage example:
    rem Execute in the working directory:
    rem path\to\delft3d\installation\x64\dflowfm\scripts\run_dfmoutput.bat

setlocal enabledelayedexpansion

    rem
    rem Read arguments

    rem --help:
if [%1] EQU [--help] ( goto usage )


    rem Sets the number of threads if it is not defined
if defined OMP_NUM_THREADS (
echo OMP_NUM_THREADS is already defined
) else ( 
   rem Getting and setting the number of physical cores  
   for /F "tokens=2 delims==" %%C in ('wmic cpu get NumberOfCores /value ^| findstr NumberOfCores') do set NumberOfPhysicalCores=%%C
   set /A OMP_NUM_THREADS=!NumberOfPhysicalCores! - 2
   if /I OMP_NUM_THREADS LEQ 2 ( set OMP_NUM_THREADS=2 )
)
echo OMP_NUM_THREADS is %OMP_NUM_THREADS%

set workdir=%CD%
echo Working directory: %workdir%
    rem
    rem Set the directories containing the binaries
    rem
set D3D_HOME=%~dp0..\..\..

    rem Remove "\dflowfm\scripts\..\..\.." from D3D_HOME
set D3DT=%D3D_HOME:~0,-25%
echo D3D_HOME         : %D3D_HOME%
    rem last directory will be the architecture directory
for %%f in ("%D3DT%") do set ARCH=%%~nxf
echo ARCH             : %ARCH%

set dflowfmexedir=%D3D_HOME%\%ARCH%\dflowfm\bin
set sharedir=%D3D_HOME%\%ARCH%\share\bin


    rem
    rem No adaptions needed below
    rem

    rem Run
set PATH=%dflowfmexedir%;%sharedir%
echo executing: "%dflowfmexedir%\dfmoutput.exe" --verbose %1 %2 %3 %4 %5 %6 %7 %8 %9
"%dflowfmexedir%\dfmoutput.exe" --verbose %1 %2 %3 %4 %5 %6 %7 %8 %9

goto end

:usage
echo Usage:
echo run_dfmoutput.bat [--help] [dfmoutput options]
echo     --help            : (Optional) show this usage
echo     dfmoutput options : (Optional) arguments to pass through to dfmoutput

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
