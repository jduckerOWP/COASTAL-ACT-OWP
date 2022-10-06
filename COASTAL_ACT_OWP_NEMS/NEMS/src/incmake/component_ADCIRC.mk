########################################################################

# Location of the ESMF makefile fragment for this component:
adcirc_mk = $(ADCIRC_BINDIR)/adcirc.mk
all_component_mk_files+=$(adcirc_mk)

# Location of source code and installation
ADCIRC_SRCDIR?=$(ROOTDIR)/ADCIRC
ADCIRC_BINDIR?=$(ROOTDIR)/ADCIRC_INSTALL
ADCIRC_NUOPC_SRCDIR?=$(ROOTDIR)/ADCIRC/thirdparty/nuopc

# Make sure the expected directories exist and are non-empty:
$(call require_dir,$(ADCIRC_SRCDIR),ADCIRC source directory)

# ENV for ADCIRC - exchange with NEMS ENV
comp_option=$(NEMS_COMPILER)

ADCIRC_ALL_OPTS= \
  COMP_SRCDIR="$(ADCIRC_SRCDIR)" \
  COMP_BINDIR="$(ADCIRC_BINDIR)" \
  MACHINE_ID="$(MACHINE_ID)" \
  compiler=$(comp_option)

########################################################################

# Rule for building this component:

build_ADCIRC: $(adcirc_mk)


$(adcirc_mk): configure $(CONFDIR)/configure.nems
	+$(MODULE_LOGIC) ; cd $(ADCIRC_NUOPC_SRCDIR); exec ./make_nuopc.sh $(comp_option)
	+$(MODULE_LOGIC) ; cd $(ADCIRC_NUOPC_SRCDIR) ; exec $(MAKE) $(ADCIRC_ALL_OPTS) nuopcinstall \
          DESTDIR=/ "INSTDIR=$(ADCIRC_BINDIR)"
	@echo ""
	test -d "$(ADCIRC_BINDIR)"
	@echo ""
	test -s $(adcirc_mk)
	@echo ""

########################################################################

# Rule for cleaning the SRCDIR and BINDIR:

clean_ADCIRC_NUOPC:
	+cd $(ADCIRC_NUOPC_SRCDIR); exec rm -f *.o *.mod
	@echo ""

distclean_ADCIRC_NUOPC: clean_ADCIRC_NUOPC
	+cd $(ADCIRC_NUOPC_SRCDIR) ; exec rm -f libadc_cap.a adcirc.mk
	@echo ""

clean_ADCIRC: clean_ADCIRC_NUOPC
	+cd $(ADCIRC_SRCDIR)/work ; exec $(MAKE) -k clean
	@echo ""

distclean_ADCIRC: distclean_ADCIRC_NUOPC
	+cd $(ADCIRC_SRCDIR)/work ; exec $(MAKE) -k clobber
	rm -rf $(ADCIRC_BINDIR)
	@echo ""
