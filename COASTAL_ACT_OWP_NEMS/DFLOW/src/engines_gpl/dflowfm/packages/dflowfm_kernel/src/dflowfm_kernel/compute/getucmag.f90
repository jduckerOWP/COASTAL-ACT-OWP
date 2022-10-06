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

! $Id: getucmag.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/getucmag.f90 $

!> Computes the velocity magnitude in cell centers, typically used for output only.
!! All arrays via input arguments, not via use m_flow.
subroutine getucmag(N, ucxi, ucyi, ucmago)
   use m_flowgeom, only: ndx
   use m_flow, only: kmx

   implicit none
   integer,          intent(in   ) :: N         !< Length of cell arrays (probably ndkx)
   double precision, intent(in   ) :: ucxi(N)   !< Input array containing cell centered x-velocities.
   double precision, intent(in   ) :: ucyi(N)   !< Input array containing cell centered y-velocities.
   double precision, intent(  out) :: ucmago(N) !< Output array containing cell centered velocity magnitudes.

   integer          :: kk,k,kb,kt

   !call realloc(ucmag, ndkx, keepExisting = .false.)
   ! NOTE: workx/y contain the velocity vectors, possibly corrected into Eulerian velocities (see getucxucyeuler).
   if ( kmx.gt.0 ) then
      do kk=1,ndx
         call getkbotktop(kk,kb,kt)
         do k = kb,kt
            ucmago(k) = sqrt(ucxi(k)**2 + ucyi(k)**2) ! TODO: this does not include vertical/w-component now.
         end do
      end do
   else
      do kk = 1,ndx
            ucmago(kk) = sqrt(ucxi(kk)**2 + ucyi(kk)**2)
      enddo
   end if

end subroutine getucmag
