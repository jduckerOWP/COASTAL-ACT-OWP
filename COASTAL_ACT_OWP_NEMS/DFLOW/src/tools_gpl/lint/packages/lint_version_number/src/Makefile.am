#-------------------------------------------------------------------------------
#   Makefile for version_number
#
#   jan.mooiman@deltares.nl
#   22 jul 2008
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = liblint_version_number.la

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../include 

#-------------------------------------------------------------------------------
#Sources
liblint_version_number_la_SOURCES = \
         c2c.c \
         f2c.c \
         version.c

BUILT_SOURCES =  ../include/always \
	../include/version_number.h

../include/always:
	rm -f ../include/version_number.h

../include/version_number.h:
	$(top_srcdir)/scripts_lgpl/linux/make_revision.sh $(top_builddir) ../../.. ../include/version_number.ini ../include/version_number.h.svn ../include/version_number.h
#-------------------------------------------------------------------------------
#Libraries

