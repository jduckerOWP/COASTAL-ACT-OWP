#define FILENAME "NWM_ESMF_Utility.F90"
#define MODNAME "NWM_ESMF_Utility.F90"
#include "NWM_NUOPC_Macros.h"

#define DEBUG=off

!-------------------------------------------------------------------------------
! A test coupled application utility module
!
! Author:
!   Tim Campbell
!   Naval Research Laboratory
!   November 2014
!   Beheen M. Trimble, Lynker Tech, NOAA, 8/19/2020
!-------------------------------------------------------------------------------

module NWM_ESMF_Utility

  use ESMF
  use NUOPC

  implicit none
  save
  private


  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
#undef METHOD
#define METHOD "NWM_Template"
  
  subroutine NWM_Template()
    ! Return object-wide information from a LocStream

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    !rc = ESMF_SUCCESS


#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

end module NWM_ESMF_Utility
