#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 jan 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libio.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/engines_gpl/wave/packages/data/src

AM_FCFLAGS+=${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}

#-------------------------------------------------------------------------------
#Sources
libio_la_SOURCES = \
         compvel.f90 \
         cp_file.f90 \
         dam_cod.f90 \
         deletehotfile.f90 \
         flow2wav.f90 \
         get_cur.f90 \
         get_dep.f90 \
         get_flow_fields.f90 \
         get_lev.f90 \
         get_params.f90 \
         get_swan_depth.f90 \
         get_var_netcdf.f90 \
         get_vegi_map.f90 \
         get_visc.f90 \
         get_wind.f90 \
         grmap_esmf.f90 \
         out2d.f90 \
         postprocess_ice.f90 \
         put_wave_fields.f90 \
         read_swan_output.f90 \
         readn.f90 \
         scan_fl.f90 \
         wavcur2d.f90 \
         wavenr_wave.f90 \
         write_bnd_loc.f90 \
         write_swan_datafile.f90 \
         write_wave_grid_netcdf.f90 \
         write_wave_his_netcdf.f90 \
         write_wave_map.f90 \
         write_wave_map_netcdf.f90 \
         write_wave_map_ice.f90 \
         write_wave_map_wind.f90 \
         writek.f90 \
         wrivek.f90 \
         wrpara.f90

#-------------------------------------------------------------------------------
#Libraries

# Manualy add the netcdff (fortran dll), bug in pkg-config file is not fixed until in NetCDF 4.4 HEAD
LIBS+=${NETCDF_LIBS} ${NETCDF_FORTRAN_LIBS} -lnetcdff

