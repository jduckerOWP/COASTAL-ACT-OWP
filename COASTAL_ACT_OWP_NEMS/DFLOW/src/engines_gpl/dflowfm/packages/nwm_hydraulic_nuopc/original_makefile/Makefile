#
# --- NWM Hydraulic engine makefile 
# --- Requires ESMVv8+
# --- NWM Hydraulic ESMF component.
#

# ###############
# Local Variables
# ###############

HR    := " "
HR    := $(HR)$(HR)
COMMA := ,
DIR   := $(CURDIR)

#VPATH := ./lib

# ###########################
# Include ESMFMKFILE fragment
# ###########################

ifneq ($(origin ESMFMKFILE), environment)
  $(warning Environment variable ESMFMKFILE was not set.)
  export ESMFMKFILE=/scratch1/NCEPDEV/nems/emc.nemspara/soft/esmf/8.0.0bs48g-intel18.0.5.274-impi2018.0.4-netcdf4.6.1/lib/esmf.mk
  $(info Setting to $(ESMFMKFILE))
endif
include $(ESMFMKFILE)

# #########################
# Determine Build Precision
# #########################

#ifeq ($(BUILD_PREC),r4)
#override ESMF_F90COMPILECPPFLAGS += -DREAL8
#else ifeq ($(BUILD_PREC),r8)
#override ESMF_F90COMPILECPPFLAGS += -DREAL8
#else
#override ESMF_F90COMPILECPPFLAGS += -DREAL8
#endif
override ESMF_F90COMPILECPPFLAGS += -DREAL8

override ESMF_F90COMPILECPPFLAGS += -r8
# #################################
# Compile with Debugging Directives
# #################################

ifeq ($(DEBUG),on)
override ESMF_F90COMPILECPPFLAGS += -DDEBUG
override ESMF_CXXCOMPILECPPFLAGS += -DDEBUG
endif

# ###########################
# Determine Installation Path
# ###########################

ifndef DESTDIR     # this var is coming from NEMS/src/incmake/component_DFLOW.mk
  $(info Environment variable DESTDIR was not set.)
  DESTDIR  := $(DIR)
  $(info Setting to $(DESTDIR) )
else
  $(info DESTDIR=$(DESTDIR))
endif


ifndef INSTDIR     # this var is coming from NEMS/src/incmake/component_DFLOW.mk
  $(warning Environment variable INSTDIR was not set.)
  INSTDIR  := DFLOW_$(shell date '+%Y-%m-%d-%H-%M-%S')
  $(info Setting to $(INSTDIR) )
else
  $(info INSTDIR=$(INSTDIR) )
endif


ifndef INSTPATH   
  $(warning Environment variable INSTPATH was not set.)
  INSTPATH := $(abspath $(DESTDIR)/$(INSTDIR))
  $(info Setting to $(INSTPATH) )
else
  $(info INSTPATH=$(INSTPATH) )
endif


# ###############
# Model Variables
# ###############

ifndef COMP_SRCDIR      # this var is coming from NEMS/src/incmake/component_DFLOW.mk
  $(warning Environment variable COMP_SRCDIR was not set.)
  MODEL_DIR    := $(abspath $(DIR))
  $(info Setting to $(MODEL_DIR) )
else 
  MODEL_DIR    := $(abspath $(COMP_SRCDIR))
  $(info Environment variable COMP_SRCDIR=$(MODEL_DIR) )
endif

MODEL_DIR := /scratch2/COASTAL/coastal/noscrub/Jason.Ducker/DFlowFM_2022.03/src
MODEL_BUILD_DIR := /scratch2/COASTAL/coastal/noscrub/Jason.Ducker/DFlowFM_2022.03/build_all

MODEL_LIBS := $(INSTDIR)/libdflowfm.so 
	      #$(INSTDIR)/libdflowfm_kernel.a \
              $(INSTDIR)/libec_module.a 
	      #$(INSTDIR)/libdeltares_common.a \
              $(INSTDIR)/libdeltares_common_c.a \
              $(INSTDIR)/libdelftio.a \
              $(INSTDIR)/libdelftio_shm.a \
              $(INSTDIR)/libio_netcdf.a \
              $(INSTDIR)/libio_netcdf.a \
              $(INSTDIR)/libpart_io_f.a \
              $(INSTDIR)/libpart_utils_f.a \
              $(INSTDIR)/libpart_data_f.a \
              $(INSTDIR)/libesmfsm_c.a \
              $(INSTDIR)/libesmfsm_version_number.a \
              $(INSTDIR)/libgridgeom.a \
              $(INSTDIR)/libnefis.a \
              $(INSTDIR)/libwaq_kernel.a \
              $(INSTDIR)/libwave_kernel.a \
	      $(INSTDIR)/libexpat.a \
	      $(INSTDIR)/liblibsigwatch.a
	      #$(INSTDIR)/libdflowfm.so \
	      $(INSTDIR)/libwaq_plugin_wasteload.so \
	      $(INSTDIR)/libwave.so \
	      $(INSTDIR)/libdimr.so \
              $(INSTDIR)/libwaq_io.a \
              $(INSTDIR)/libwaq_process.a \
              $(INSTDIR)/libwaq_utils_c.a \
              $(INSTDIR)/libwaq_utils_f.a \

MODEL_LIB := $(INSTDIR)/libdflowfm_kernel.a

MODEL_MODS := -I $(MODEL_BUILD_DIR)/dflowfm_lib/ \
	-I $(MODEL_BUILD_DIR)/dflowfm_kernel/ \
        -I $(MODEL_BUILD_DIR)/ec_module/ \
        -I $(MODEL_BUILD_DIR)/gridgeom/ \
	-I $(MODEL_BUILD_DIR)/deltares_common/ \
        -I $(MODEL_BUILD_DIR)/delwaq/ \
        -I $(MODEL_BUILD_DIR)/waq_kernel/ \
        -I $(MODEL_BUILD_DIR)/waq_plugin_wasteload/ \
        -I $(MODEL_BUILD_DIR)/wave/ \
        -I $(MODEL_BUILD_DIR)/wave_kernel/ \
	-I $(MODEL_BUILD_DIR)/nefis/ \
        -I $(PETSC_LIB_PATH)/../include/ \
        -I $(METIS_LIB_PATH)/../include/ \
        -I $(PROJ_LIB_PATH)/../include/ \
        -I $(GDAL_LIB_PATH)/../include/ \
        -I $(NETCDF_LIB_PATH)/../include/ 


# #############
# Cap Variables
# #############

CAP_DIR       := $(abspath $(MODEL_DIR)/engines_gpl/dflowfm/packages/nwm_hydraulic_nuopc)
CAP_LIB       := libnwm_hyc.a
CAP_DEP_FRONT := nwm_hyc_cap          # for NEMS, must be same name as its module
CAP_VERS      := VERSION
CAP_MK        := dflow.mk             # for NEMS must be same as in components_DFLOW.mk

CAP_OBJS      := NWM_Hydraulic_Cap.o
CAP_OBJS      += NWM_Hydraulic_Gluecode.o
CAP_OBJS      += NWM_ESMF_Extensions.o
CAP_OBJS      += NWM_ESMF_Utility.o

CAP_MODS      := nwm_esmf_extensions.mod  nwm_hyc_cap.mod  nwm_hyc_gluecode.mod nwm_esmf_utility.mod 

CAP_FILES     := $(CAP_OBJS) $(CAP_LIB) $(CAP_MODS) $(CAP_VERS) $(CAP_MK)


# #######################
# Primary Makefile Target
# #######################
.PHONY: nuopc nuopcinstall nuopcdistclean nuopcclean install_mk dust


nuopc: $(CAP_FILES)

nuopcinstall: $(CAP_LIB) $(CAP_MODS) $(CAP_VERS) \
	$(addprefix $(CAP_DIR)/,$(CAP_LIB)) \
	$(addprefix $(CAP_DIR)/,$(CAP_MODS)) \
	$(addprefix $(CAP_DIR)/,$(CAP_VERS)) \
	install_mk

# ############
# Dependencies
# ############

NWM_Hydraulic_Cap.o: NWM_Hydraulic_Gluecode.o NWM_ESMF_Extensions.o NWM_NUOPC_Macros.h
NWM_Hydraulic_Gluecode.o: NWM_ESMF_Extensions.o NWM_ESMF_Utility.o NWM_NUOPC_Macros.h


# ##############
# Build Settings
# ##############

.SUFFIXES: 
.SUFFIXES: .c .C .f90 .F90 .F .f

.F:
	@echo "Must have an explicit rule for" $*
.f:
	@echo "Must have an explicit rule for" $*
.C:
	@echo "Must have an explicit rule for" $*
.c: 
	@echo "Must have an explicit rule for" $*

%.o : %.f90
	@echo $(HR)
	@echo "Compiling $@..."
	@echo
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<

%.o : %.F90
	@echo $(HR)
	@echo "Compiling $@..."
	@echo
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) $<  \
	-DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) -I .  $(MODEL_MODS)
        

# #####################
# Build NUOPC Component
# #####################

$(CAP_LIB): $(CAP_OBJS)
	@echo $(HR)
	@echo "Creating static library $@..."
	@echo
	ar cr  $@ $(CAP_OBJS); \
        ranlib $@ ; chmod +x $@; 


$(CAP_VERS):
	@echo $(HR)
	@echo "Generating Version Information"
	@echo
	@echo "# Hydraulic Cap Version" > $(CAP_VERS)
	@if [ -d .svn ]; then \
	  echo "SVN Repository" > $(CAP_VERS); \
	  svn info . | grep URL >> $(CAP_VERS); \
	  svn info . | grep "Last Changed Rev" >> $(CAP_VERS); \
	elif [ -d .git ]; then \
	  echo "Git Repository" > $(CAP_VERS); \
	  git show . | grep -m 1 "commit " >> $(CAP_VERS); \
	  git show . | grep -m 1 "Author: " >> $(CAP_VERS); \
	  git show . | grep -m 1 "Date: " >> $(CAP_VERS); \
	fi

$(CAP_MK): 
	@echo $(HR)
	@echo "Generating Hydraulic NUOPC Makefile Fragment"
	@echo
	@echo "# ESMF self-describing build dependency makefile fragment" > $(CAP_MK)
	@echo "" >> $(CAP_MK)
	@echo "ESMF_DEP_FRONT     = $(CAP_DEP_FRONT)" >> $(CAP_MK)
	@echo "ESMF_DEP_INCPATH   = $(CAP_BINDIR)" >> $(CAP_MK)
	@echo "ESMF_DEP_CMPL_OBJS = " >> $(CAP_MK)
	@echo "ESMF_DEP_LINK_OBJS = $(CAP_DIR)/$(CAP_LIB)" >> $(CAP_MK)
	@echo "ESMF_DEP_SHRD_PATH = " >> $(CAP_MK)
	@echo "ESMF_DEP_SHRD_LIBS = " >> $(CAP_MK)

# -----------------------------------------------------------------------------
# Install Library, Modules, and Makefile Fragment
# -----------------------------------------------------------------------------

$(INSTPATH)/%:
	@echo $(HR)
	@echo "Installing $(notdir $@)"
	@echo
	@mkdir -p $(INSTPATH)
	cp $(notdir $@) $@

# ##########
# # Below variables are read by NEMS/src/GNUMakefile in this manner:
# # DEP_FRONTS    := $$(DEP_FRONTS) -DFRONT_$(1)=$$(ESMF_DEP_FRONT)
# # DEP_INCS      := $$(DEP_INCS) $$(addprefix -I, $$(ESMF_DEP_INCPATH))
# # DEP_CMPL_OBJS := $$(DEP_CMPL_OBJS) $$(ESMF_DEP_CMPL_OBJS)
# # DEP_LINK_OBJS := $$(DEP_LINK_OBJS) $$(ESMF_DEP_LINK_OBJS)
# # DEP_SHRD_PATH := $$(DEP_SHRD_PATH) $$(addprefix -L, $$(ESMF_DEP_SHRD_PATH)) $$(addprefix -Wl$$(comma)-rpath$$(comma), $$(ESMF_DEP_SHRD_PATH))
# # DEP_SHRD_LIBS := $$(DEP_SHRD_LIBS) $$(addprefix -l, $$(ESMF_DEP_SHRD_LIBS))
# NOTE: libraries have order. Watch
# # ##########
#
install_mk:
	@echo $(HR)
	@echo "Installing NUOPC Makefile Fragment"
	@mkdir -p $(INSTPATH)
	@echo "# ESMF self-describing build dependency makefile fragment" > $(INSTPATH)/$(CAP_MK)
	@echo "" >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_FRONT     = $(CAP_DEP_FRONT)" >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_INCPATH   = $(INSTPATH)" >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_CMPL_OBJS = " >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_LINK_OBJS = -L$(METIS_LIB_PATH) -lmetis -L$(PETSC_LIB_PATH) -lpetsc -L$(GDAL_LIB_PATH) -lgdal -L$(PROJ_LIB_PATH) -lproj $(INSTPATH)/$(CAP_LIB) $(MODEL_LIBS)" >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_SHRD_PATH = " >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_SHRD_LIBS = " >> $(INSTPATH)/$(CAP_MK)
	cp -f $(CAP_LIB) $(CAP_MODS) $(INSTPATH)
	cp -f $(CAP_DIR)/lib/* $(INSTPATH)
	@echo $(HR)


# ###########
# Check Build
# ###########

define checkfile
	@if [ ! -e $(1) ]; then \
	echo "File is missing:$(1)"; \
	exit 1; fi;
endef # blank line in checkfile is required

define checkdir
	@if [ ! -d $(1) ]; then \
	echo "Directory is missing:$(1)"; \
	exit 1; fi;
endef # blank line in checkdir is required

check: check_esmf

# ##################
# Check ESMF Version
# ##################

check_esmf:
	@echo $(HR)
	@echo "Checking ESMFMKFILE file..."
	@echo
	@echo "ESMFMKFILE=$(ESMFMKFILE)"
	@if [ "$(ESMF_VERSION_MAJOR)" -lt 8 ]; then \
	echo "Please use ESMF version 8+"; \
	exit 1; fi;
	@echo "ESMF Version=$(ESMF_VERSION_STRING)"


# Clean Cap and Model
# ###################

nuopcdistclean: nuopcclean
	@echo $(HR)
	@echo "Cleaning Model build..."             # used for local installation/build
	@echo ""
	$(call checkdir, $(MODEL_DIR))
	make -C $(MODEL_DIR) -f $(MODEL_MK) clean PET*.ESMF_LogFile *.nc
	@echo $(HR)

# #########
# Clean Cap
# #########

nuopcclean:
	@echo $(HR)
	@echo "Cleaning Cap build..."
	@echo
	rm -f $(CAP_FILES)
	rm -rf $(CAP_DIR)/lib $(CAP_DIR)/bin $(CAP_DIR)/share
	@echo $(HR)

dust:
	@echo $(HR)
	@echo "Dusting ..."
	@echo
	rm -f PET*.ESMF_LogFile slurm* unstruc.dia
	rm -rf DFM_OUTPUT_*
	@echo $(HR)

# ------------------------------------------------------------------------------
