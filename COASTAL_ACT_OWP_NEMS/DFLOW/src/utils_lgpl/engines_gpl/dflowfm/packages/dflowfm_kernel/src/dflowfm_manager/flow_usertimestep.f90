!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2022.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id: flow_usertimestep.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_manager/flow_usertimestep.f90 $

!> A complete single user time step (init-run-finalize).
 subroutine flow_usertimestep(key, iresult)                   ! do computational flowsteps until timeuser
 use m_flowtimes
 use timers
 use unstruc_messages
 use m_partitioninfo
 use unstruc_display, only: jaGUI
 use m_timer
 use dfm_error
 implicit none
   integer, intent(out) :: key     !< Key number if any key was pressed in GUI.
 integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

   call timstrt('User time loop', handle_user)

   iresult = DFM_GENERICERROR
   key = 0

   call flow_init_usertimestep(iresult)
   if (iresult /= DFM_NOERR) then
      goto 888
   end if

   call flow_run_usertimestep(key, iresult)
   if (iresult /= DFM_NOERR) then
      goto 888
   end if

   call flow_finalize_usertimestep(iresult)
   if (iresult /= DFM_NOERR) then
      goto 888
   end if

   call timstop(handle_user)

   iresult = DFM_NOERR
   return ! Return with success.

888 continue
end subroutine flow_usertimestep
