################################################################################
##!! @author Saeed Moghimi (moghimis@gmail.com)
##!! @date 15/1/17 Original documentation

##!! @date 07/01/21
##!! @author Panagiotis Velissariou (panagiotis.velissariou@noaa.gov)

##! LOG !
##! 
################################################################################


###########################################################################
# Mandatory environment variables NETCDFHOME, ESMFMKFILE
ifneq ($(origin NETCDFHOME), environment)
  $(error Environment variable NETCDFHOME was not set)
endif
#-----
ifneq ($(origin ESMFMKFILE), environment)
  $(error Environment variable ESMFMKFILE was not set)
endif
#-----

include $(ESMFMKFILE)

ifneq ($(origin ADCDIR), environment)
  $(warning Environment variable ADCDIR was not set.)
  # this var is coming from NEMS/src/incmake/component_ADCIRC.mk  
  ADCDIR := $(COMP_SRCDIR)
  # ADCDIR := $(abspath ../../)           # this is same as above but independent
  $(info Setting to $(ADCDIR) ) 
endif

INCDIRS := -I. -I$(ADCDIR)/work/odircp -I$(ADCDIR)/prep
LIBDIRS := -L. -L$(ADCDIR) 
LIBSS   := -ladc  -lnetcdf  -lnetcdff -lhdf5 -lhdf5_fortran

INCDIRS += -I${NETCDFHOME}/include   
LIBDIRS += -L${NETCDFHOME}/lib

ifneq ($(origin HDF5HOME), environment)
  INCDIRS += -I${HDF5HOME}/include   
  LIBDIRS += -L${HDF5HOME}/lib
endif

# unremark this line to define petlists
LIBDIRS += -DWITHPETLISTS_on 

ifeq ($(compiler),gnu)
  LIBDIRS +=  -g -O0 -fbacktrace 
endif

ifeq ($(compiler),intel)
  LIBDIRS += -g -traceback 
endif
################################################################################
################################################################################

.SUFFIXES: .f90 .F90 .c .C

%.o : %.F90
	@echo ""
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $(INCDIRS) $(LIBDIRS) -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) $<

%.o : %.F
	@echo ""
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $(INCDIRS) $(LIBDIRS) -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) $<

.PRECIOUS: %.o

# -----------------------------------------------------------------------------
vars:
	@echo " >>>>>>>>>>>>>>>> vars >>>>>>>>>>>>>>"
	@echo "INCDIRS $(INCDIRS)"
	@echo "LIBDIRS $(LIBDIRS)"
	@echo "ESMF_F90COMPILER $(ESMF_F90COMPILER)"
	@echo "ESMF_F90LINKER $(ESMF_F90LINKER)"
	@echo "ESMF_F90LINKOPTS $(ESMF_F90LINKOPTS)"
	@echo "ESMF_F90LINKPATHS  $(ESMF_F90LINKPATHS)"
	@echo "ESMF_F90LINKRPATHS $(ESMF_F90LINKRPATHS)"
	@echo "ESMF_F90ESMFLINKLIBS $(ESMF_F90ESMFLINKLIBS)"	


# -----------------------------------------------------------------------------

PWDDIR     := $(shell pwd)
INSTALLDIR := $(INSTDIR)    # coming from NEMS/src/incmake/component_ADCIRC.mk

LIBRARY  = libadc_cap.a
MODULES  = adc_cap.o adc_mod.o couple2swan_modif.o 

nuopc: $(LIBRARY) 

$(LIBRARY): $(MODULES)
	$(AR) $(ARFLAGS) $@ $?

nuopcinstall: $(LIBRARY)
	@echo ""
	rm -f adcirc.mk
	@echo "# ESMF self-describing build dependency makefile fragment" > adcirc.mk
	@echo  >> adcirc.mk
	@echo "ESMF_DEP_FRONT     = adc_cap" >> adcirc.mk
	@echo "ESMF_DEP_INCPATH   = $(PWDDIR) $(strip $(INSTALLDIR)) "    >> adcirc.mk
	@echo "ESMF_DEP_CMPL_OBJS = " >> adcirc.mk
	@echo "ESMF_DEP_LINK_OBJS =  -L$(strip $(INSTALLDIR)) -ladc $(strip $(INSTALLDIR))/libadc_cap.a  -L$(strip $(ADCDIR))/work/  $(strip $(ADCDIR))/work/libadc.a  " >> adcirc.mk
	mkdir -p $(strip $(INSTALLDIR))
	cp -f $(strip $(ADCDIR))/work/libadc.a $(strip $(INSTALLDIR))
	cp -f libadc_cap.a adc_cap.mod $(strip $(INSTALLDIR))
	cp -f adcirc.mk $(strip $(INSTALLDIR))
	@echo ""


# module dependencies:
adc_cap.o:  couple2swan_modif.o
adc_cap.o:  adc_mod.o


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: nuopc nuopcclean nuopcinstall vars dust nuopcdistclean

nuopcclean:
	rm -f  adc_cap.o adc_mod.o couple2swan_modif.o  adc_cap.mod adc_mod.mod couple2swan_modif.mod libadc_cap.a adcirc.mk

nuopcdistclean: nuopcclean dust
	@echo ""
	@echo "Cleaning Model build..."
	@echo ""
	make -C $(ADCDIR) -f $(MODEL_MK) clean

dust:
	@echo ""
	@echo "Dusting ..."
	@echo
	rm -f PET*.ESMF_LogFile *.nc
