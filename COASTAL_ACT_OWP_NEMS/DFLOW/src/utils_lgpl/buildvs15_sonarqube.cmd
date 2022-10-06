@ echo off

setlocal enabledelayedexpansion

set globalErrorLevel=0
set d3d_open_only=0

  rem Jump to the directory where this build.cmd script is
cd %~dp0

  
  rem Quiet removal of output file build.log. Do not report any problems with this deletion
del /F/Q build*.log > del.log 2>&1
del /F/Q del.log


  rem Set environment parameters for VisualStudio
call "%VS140COMNTOOLS%..\..\VC\vcvarsall.bat" amd64

  rem The path to devenv.exe is now added to PATH: no full path specification needed on next line.


rem ===========================================================================
rem delft3d_open.sln
devenv.exe delft3d_open.sln /Build "Release|x64" /Out build_delft3d_open.log
if NOT %ErrorLevel% EQU 0 (
    echo "Error in compiling delft3d_open.sln: %ErrorLevel%"
    set globalErrorLevel=%ErrorLevel%
)
if %d3d_open_only% EQU 1 (
    echo "Skipping remaining build steps due to flag delft3d_open_only"
    goto finished
)

rem ===========================================================================
rem dflowfm_open.sln
devenv.exe dflowfm_open.sln /Build "Release|x64" /Out build_dflowfm_open.log
if NOT %ErrorLevel% EQU 0 (
    echo "Error in compiling dflowfm_open.sln: %ErrorLevel%"
    set globalErrorLevel=%ErrorLevel%
)


rem ===========================================================================
rem io_netcdf.sln
devenv.exe io_netcdf.sln /Build "Release|x64" /Out build_io_netcdf.log
if NOT %ErrorLevel% EQU 0 (
    echo "Error in compiling delft3d_open.sln: %ErrorLevel%"
    set globalErrorLevel=%ErrorLevel%
)


rem ===========================================================================
rem nefis.sln
devenv.exe nefis.sln /Build "Release|x64" /Out build_nefis.log
if NOT %ErrorLevel% EQU 0 (
    echo "Error in compiling delft3d_open.sln: %ErrorLevel%"
    set globalErrorLevel=%ErrorLevel%
)



:finished

  rem In build.log, replace "error" by TeamCity messages
third_party_open\commandline\bin\win32\sed.exe -e "/[Ee]rror[\:\ ]/s/^/\#\#teamcity\[buildStatus status\=\'FAILURE\' text\=\' /g;/buildStatus/s/$/\'\]/g" build_delft3d_open.log 
if %d3d_open_only% EQU 1 (
    echo "Skipping remaining replace steps due to flag delft3d_open_only"
    goto end
)
third_party_open\commandline\bin\win32\sed.exe -e "/[Ee]rror[\:\ ]/s/^/\#\#teamcity\[buildStatus status\=\'FAILURE\' text\=\' /g;/buildStatus/s/$/\'\]/g" build_dflowfm_open.log 
third_party_open\commandline\bin\win32\sed.exe -e "/[Ee]rror[\:\ ]/s/^/\#\#teamcity\[buildStatus status\=\'FAILURE\' text\=\' /g;/buildStatus/s/$/\'\]/g" build_io_netcdf.log 
third_party_open\commandline\bin\win32\sed.exe -e "/[Ee]rror[\:\ ]/s/^/\#\#teamcity\[buildStatus status\=\'FAILURE\' text\=\' /g;/buildStatus/s/$/\'\]/g" build_nefis.log 


:end

if NOT %globalErrorLevel% EQU 0 (
    echo An error occurred in one or more compilation steps
    echo Returning with error number %globalErrorLevel%
    exit /B %globalErrorLevel%
)


