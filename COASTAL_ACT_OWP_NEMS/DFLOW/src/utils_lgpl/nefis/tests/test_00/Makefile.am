#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   23 oct 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_PROGRAMS = test_00

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../../packages/nefis/include

test_00_SOURCES = \
	test_00.c
test_00_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(CCLIBS)
