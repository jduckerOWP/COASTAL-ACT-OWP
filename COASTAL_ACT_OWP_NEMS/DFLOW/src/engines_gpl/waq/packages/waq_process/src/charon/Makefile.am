#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libcharon.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/engines_gpl/waq/include \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils \
         -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------
#Sources
libcharon_la_SOURCES = arith.f \
       barx.f \
       berror.f \
       ceccal.f \
       chalsp.f \
       chinp2.f \
       chinpu.f \
       chmapd.f \
       chphas.f \
       chsto2.f \
       chstoc.f \
       cjcor.f \
       cjtemp.f \
       del.f \
       dexp2.f \
       divide.f \
       ehcalc.f \
       exit.f \
       find.f \
       leave.f \
       lookup.f \
       lp.f \
       matinv.f \
       matrix.f \
       partb.f \
       pdfch1.f \
       pdfch2.f \
       pdfcha.f \
       phcalc.f \
       pop.f \
       push.f \
       rcalc.f \
       rows.f \
       simple.f \
       solve2.f \
       solvex.f \
       startch.f \
       wricha.f \
       xlog.f

#-------------------------------------------------------------------------------
# Libraries
