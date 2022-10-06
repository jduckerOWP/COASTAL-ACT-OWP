@ echo off
title run_dflowfm
    rem
    rem This script runs dflowfm on Windows
    rem Adapt and use it for your own purpose
    rem
    rem When using mpich2 for the first time on a machine:
    rem Execute "smpd -install" as administrator:
    rem     Preparation: Check that your Delft3D installation contains "...\x64\share\bin\smpd.exe". Optionally copy it to a local directory (it will run as a service).
    rem     "Start" -> "All programs" -> "Accessories", right-click "Command Prompt", "Run as Administrator"
    rem     In this command box:
    rem         cd ...\x64\share\bin
    rem         smpd -install
    rem     When there is an smpd already running on the machine, it must be ended first, using the Microsoft Task Manager, 
    rem     or in the command  box: smpd -uninstall
    rem
    rem
    rem This script runs dimr in parallel mode on Windows
    rem Adapt and use it for your own purpose
    rem
    rem Usage example:
    rem Execute in the working directory:
    rem path\to\delft3d\installation\x64\dimr\scripts\run_dimr_parallel.bat
    rem More examples: check run scripts in https://svn.oss.deltares.nl/repos/delft3d/trunk/examples/*






    rem Usage example:
    rem Execute in the working directory:
    rem path\to\delft3d\installation\x64\dflowfm\scripts\run_dflowfm.bat

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
echo executing: "%dflowfmexedir%\dflowfm-cli.exe" --nodisplay --autostartstop %1 %2 %3 %4 %5 %6 %7 %8 %9
"%dflowfmexedir%\dflowfm-cli.exe" --nodisplay --autostartstop %1 %2 %3 %4 %5 %6 %7 %8 %9

goto end

:usage
echo Usage:
echo run_dflowfm.bat [--help] [dflowfm options]
echo     --help          : (Optional) show this usage
echo     dflowfm options : (Optional) arguments to pass through to dflowfm

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
