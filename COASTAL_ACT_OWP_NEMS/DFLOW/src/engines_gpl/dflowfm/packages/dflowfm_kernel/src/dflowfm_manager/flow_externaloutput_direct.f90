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

! $Id: flow_externaloutput_direct.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_manager/flow_externaloutput_direct.f90 $

!> Writes current state immediately to files, typically used in
!! case of 'emergencies', without checking output intervals.
!!
!! Writes his/map/rst data to the (existing) files.
!! Note: no timings/waq output.
subroutine flow_externaloutput_direct()
   use m_flowtimes
   use unstruc_messages
   use time_module
   implicit none
   integer :: iyear, imonth, iday, ihour, imin, isec

   call mess(LEVEL_INFO, 'Performing direct write of solution state...')

   ! Compute current absolute date time, based on time1 since refdat
   call datetime_from_refdat(time1, iyear, imonth, iday, ihour, imin, isec)
   write (msgbuf, '(a,i0,a,f12.2,a,a,a,a)') 'Simulation current time: nt = ', int(dnt, 8), ', time1 = ', time1, 's ', &
                             '(', trim(datetime_to_string(iyear, imonth, iday, ihour, imin, isec)), ').'
   call msg_flush()

   call wrimap(time1)

   call unc_write_his(time1)

   call wrirst(time1)

   call mess(LEVEL_INFO, 'Done writing solution state.')

end subroutine flow_externaloutput_direct
