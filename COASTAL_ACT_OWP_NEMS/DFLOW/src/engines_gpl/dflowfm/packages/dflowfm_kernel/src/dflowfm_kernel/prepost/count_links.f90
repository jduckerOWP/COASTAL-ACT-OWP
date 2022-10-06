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

! $Id: count_links.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/count_links.f90 $

!> count number of 2D links and 1D endpoints
subroutine count_links(mx1Dend, Nx)
   use network_data, only: numL, numL1D, kn, lne, nmk
   implicit none

   integer, intent(out) :: mx1Dend  !< number of 1D endpoints
   integer, intent(out) :: Nx       !< number of 2D links and 1D endpoints

   integer              :: k1, k2, L

   mx1Dend = 0                                        ! count MAX nr of 1D endpoints
   do L = 1,numl1D
      if ( kn(3,L) == 1 .or. kn(3,L) == 6) then       ! zeker weten
         k1 = kn(1,L) ; k2 = kn(2,L)
         if (nmk(k1) == 1 .and. nmk(k2) == 2 .and. lne(1,L) < 0 .or. &
             nmk(k2) == 1 .and. nmk(k1) == 2 .and. lne(2,L) < 0 ) then
             mx1Dend = mx1Dend + 1
         endif
      endif
   enddo


   Nx = numL + mx1Dend

   return
end subroutine count_links
