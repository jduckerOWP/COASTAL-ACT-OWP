include $(top_srcdir)/common.am
bin_PROGRAMS = kubint

kubint_SOURCES = \
	main.cpp

#-------------------------------------------------------------------------------
#Libraries 

kubint_LDADD = \
	$(top_builddir)/tools_gpl/kubint/packages/kubint_f/src/libkubint_f.la \
	$(top_builddir)/tools_gpl/kubint/packages/kubint_version_number/src/libkubint_version_number.la \
	-ldl \
	$(FCLIBS)
