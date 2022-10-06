!=========================================================================================
! NWM ESMF Extensions Module
!=========================================================================================
! CONFIGURATION IDENTIFICATION $HeadURL$
! CONFIGURATION IDENTIFICATION @(#)$Id$
!=========================================================================================

! ESMF macros for logging
#define FILENAME "NWM_ESMF_Extensions.F90"
#define MODNAME "Extension"
#include "NWM_NUOPC_Macros.h"

#define DEBUG=on

!=========================================================================================
! NWM ESMF Extensions Module
!=========================================================================================
module NWM_ESMF_Extensions

  use ESMF
  use NUOPC
  use NETCDF

  implicit none

  private

  
  character(len=ESMF_MAXSTR) :: logMsg
!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

end module NWM_ESMF_Extensions
