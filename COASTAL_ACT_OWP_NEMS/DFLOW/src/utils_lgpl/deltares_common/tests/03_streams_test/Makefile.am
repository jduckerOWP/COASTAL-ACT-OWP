include $(top_srcdir)/common.am
bin_PROGRAMS = stream02test

stream02test_SOURCES = \
	streamtest.cpp

#-------------------------------------------------------------------------------
#Includes
INCLUDES = \
	-I$(top_builddir)/utils_lgpl/stream/include

#-------------------------------------------------------------------------------
#Libraries

stream02test_LDADD = \
	$(top_builddir)/utils_lgpl/stream/packages/stream/src/libstream.la \
	$(top_builddir)/utils_lgpl/stream/packages/stream_version_number/src/libstream_version_number.la


#	-ldl \
#	$(FCLIBS) -limf
