# ESMF self-describing build dependency makefile fragment

ESMF_DEP_FRONT     = adc_cap
ESMF_DEP_INCPATH   = /scratch2/COASTAL/coastal/noscrub/Jason.Ducker/NEMS_DFlow_new/ADCIRC/thirdparty/nuopc /scratch2/COASTAL/coastal/noscrub/Jason.Ducker/NEMS_DFlow_new/ADCIRC_INSTALL 
ESMF_DEP_CMPL_OBJS = 
ESMF_DEP_LINK_OBJS =  -L/scratch2/COASTAL/coastal/noscrub/Jason.Ducker/NEMS_DFlow_new/ADCIRC_INSTALL -ladc /scratch2/COASTAL/coastal/noscrub/Jason.Ducker/NEMS_DFlow_new/ADCIRC_INSTALL/libadc_cap.a  -L/scratch2/COASTAL/coastal/noscrub/Jason.Ducker/NEMS_DFlow_new/ADCIRC/work/  /scratch2/COASTAL/coastal/noscrub/Jason.Ducker/NEMS_DFlow_new/ADCIRC/work/libadc.a  
