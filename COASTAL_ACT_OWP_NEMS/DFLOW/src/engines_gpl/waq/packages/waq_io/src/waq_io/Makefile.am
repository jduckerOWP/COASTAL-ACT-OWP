#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libwaq_io.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/engines_gpl/waq/include \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/dlwq_netcdf \
        -I$(top_builddir)/engines_gpl/part/packages/io_f/src \
        -I$(top_builddir)/engines_gpl/part/packages/utils_f/src \
        -I$(top_builddir)/engines_gpl/part/packages/data_f/src \
	    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token \
	    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

AM_FCFLAGS += $(FINCLUDES) $(FMODULES) ${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}
AM_FFLAGS  += $(FINCLUDES) $(FMODULES) ${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}

#-------------------------------------------------------------------------------
#Sources
libwaq_io_la_SOURCES = bound.f \
       pointr.f \
       check.f \
       conver.f \
       defout.f \
       dlwq01.f \
       dlwq02.f \
       dlwq03.f \
       dlwq04.f \
       dlwq05.f \
       dlwq06.f \
       dlwq07.f \
       dlwq08.f \
       dlwq09.f \
       dlwq0f.f \
       dlwq0i.f \
       dlwq5a.f \
       dlwq5b.f \
       dlwq5c.f \
       dlwq5d.f \
       dlwq5e.f \
       dlwq5g.f \
       dlwq5h.f \
       dlwq7a.f \
       dlwqj2.f \
       dlwqj3.f \
       dmpare.f \
       fmread.f \
       getopo.f \
       grid.f \
       interpol.f \
       matrix.f \
       opt0.f \
       opt1.f \
       opt2.f \
       opt3.f \
       outboo.f \
       pointb.f \
       pointi.f \
       rdodef.f \
       rdpoin.f \
       read_hydfile.f90 \
       readmp.f \
       rearaa.f \
       rwfunc.f \
       rwfun2.f \
       scale.f \
       timer.f \
       uniset.f \
       varpoi.f \
       read_grid.f \
       read_nobottomlay.f \
       read_proc_time.f \
       read_opt2.f \
       read_opt3.f \
       read_initials.f \
       read_block.f \
       read_data.f \
       read_data_ods.f \
       print_matrix.f \
       compute_matrix.f \
       compact_usefor.f \
       read_header.f \
       read_items.f \
       read_time_delay.f \
       read_sub_procgrid.f


#-------------------------------------------------------------------------------
# Libraries
