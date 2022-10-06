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

! $Id: get_isub.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/get_isub.f90 $

integer function get_isub(j, nfac1, j_loc)   !< gets the subinterval of grid layer j
   use m_spline2curvi

   implicit none

   integer,                     intent(in)  :: j       !< grid layer index
   integer, dimension(Nsubmax), intent(in)  :: nfac1   !< subinterval lengths
   integer,                     intent(out) :: j_loc   !< grid layer in the subinterval

   integer                                  :: isum, isub

   j_loc = j-1
   if ( j.gt.sum(nfac1) ) then
      isub = 0
      goto 1234
   end if

   isub = 1
   isum = 1+nfac1(isub)
   do while ( isum.le.j .and. isub.lt.Nsubmax )
      isub = isub + 1
      isum = isum + nfac1(isub)
   end do

   j_loc= j-isum+nfac1(isub)

!  error handling
1234 continue

   get_isub = isub

   return
end function
