#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   23 oct 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_PROGRAMS = test_06_dependent

test_06_dependent_SOURCES = \
	test_06.f

test_06_dependent_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(FCLIBS)
