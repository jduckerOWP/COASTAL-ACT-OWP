########################################################################

# This is a template for a component makefile fragment.  In this case,
# the component is "FOO" and so the file would be renamed to
# component_FOO.mk.

# The build and clean rules are set to those of the "build_std"
# function in the old NEMSAppBuilder.  For a different build system,
# the $(foo_mk) and clean_FOO rules.

# Note that in the build and clean rules, any shell variables must
# have double dollar signs ($$) to prevent Make from interpreting them.

# Also note that the commands in the build or clean rules must be a
# single valid shell command line, hence the "; \" at the end of every
# statement.

# clean_FOO: 
# 	something $$happens ; \
# 	something $$else $$happens

# The correct way to run a submake is this:
#
#   +$(MODULE_LOGIC) ; exec $(MAKE) -C $(FOO_SRCDIR) $(FOO_ALL_OPTS) target
#
# If you don't need modules, and you don't care if the "make" succeeds
# (such as when you're cleaning), then this works:
#
#   +-$(MAKE) -C $(FOO_SRCDIR) $(FOO_ALL_OPTS) clean
#
# Meanings of those characters:
#
#   +      = this rule runs "make." Ensures job server (-j) is passed on
#   -      = ignore the exit status of this rule (as if -k was used)
#   $(MAKE) = the path to the "make" program used to run this makefile
#   $(MODULE_LOGIC) = load modules or source the modulefile, if relevant
#   exec $(MAKE)    = the shell process is replaced by "make."  This is
#                     needed to correctly pass on the job server information
#   -C $(FOO_SRCDIR) = cd to $(FOO_SRCDIR) before running make
#   $(FOO_ALL_OPTS)  = pass on all FOO options from the $(FOO_ALL_OPTS) variable
#   target or clean  = the "make" target to build

########################################################################

# Location of the ESMF makefile fragment for this component:
dflow_mk = $(DFLOW_BINDIR)/dflow.mk
all_component_mk_files+=$(dflow_mk)
#
# Location of source code and installation(expected to be same level as NEMS!!)
DFLOW_SRCDIR?=/scratch2/COASTAL/coastal/noscrub/Jason.Ducker/DFlowFM_2022.03/src
DFLOW_BINDIR?=$(ROOTDIR)/DFLOW_INSTALL
# DFLOW_MACHINE_ID="$(MACHINE_ID)"

DFLOW_CAPDIR=$(DFLOW_SRCDIR)/engines_gpl/dflowfm/packages/nwm_hydraulic_nuopc

#
# Make sure the expected directories exist and are non-empty:
$(call require_dir,$(DFLOW_SRCDIR),DFLOW source directory)
#


# Rule for building this component:
build_DFLOW: $(dflow_mk)

DFLOW_ALL_OPTS= \
   COMP_SRCDIR="$(DFLOW_SRCDIR)" \
   COMP_BINDIR="$(DFLOW_BINDIR)" \


$(dflow_mk): configure $(CONFDIR)/configure.nems
	@echo ""
	+$(MODULE_LOGIC) ; cd $(DFLOW_SRCDIR) ; exec ./build_dflow4_nuopc.sh
	@echo "Finished building dflowfm for nuopc ....."
	@echo ""
	+$(MODULE_LOGIC) ; cd $(DFLOW_CAPDIR) ; \
			exec $(MAKE) nuopcinstall $(DFLOW_ALL_OPTS) \
	                   DESTDIR=/ "INSTDIR=$(DFLOW_BINDIR)"
	@echo "Finished building dflownuopc ....."
	@echo ""
	test -d "$(DFLOW_BINDIR)"
	@echo ""
	test -s $(dflow_mk)

# Rule for cleaning the SRCDIR and BINDIR:
clean_DFLOW:
	@echo ""
	+-cd $(DFLOW_SRCDIR) #; \
			#exec ./clean.sh  
	@echo "No need to clean D-Flow, system will recompile new files...."
	@echo "Finished cleaning dflowfm ....."
	@echo ""
	+-cd $(DFLOW_CAPDIR) ; \
			exec $(MAKE) nuopcclean
	@echo "Finished cleaning dflownuopc ....."
	@echo ""
	
distclean_DFLOW: clean_DFLOW
	@echo ""
	rm -rf $(DFLOW_BINDIR)
	@echo "Finished cleaning $(DFLOW_BINDIR) ....."
	@echo ""

