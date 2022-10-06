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

! $Id: getucxucyeuler.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/getucxucyeuler.f90 $

!> Computes the Eulerian horizontal velocities.
!! In absence of waves, these are equal to the Lagrangian ucx/ucy.
!! The Stokes drift on links is averaged to cell centers using the Perot weights.
subroutine getucxucyeuler(N, ucxeu, ucyeu)
   use m_flowgeom
   use m_flow
   use m_waves, only: ustokes            ! available for all wave models

   implicit none

   integer,          intent(in   ) :: N        !< Length of cell arrays (probably ndkx)
   double precision, intent(  out) :: ucxeu(N) !< Target array in which to store Eulerian x-velocities
   double precision, intent(  out) :: ucyeu(N) !< Target array in which to store Eulerian y-velocities

   integer          :: i, Lb, Lt, L, LL, k, k1, k2
   double precision :: u1l, wcxu, wcyu, ueul

   ucxeu(1:ndkx) = ucx(1:ndkx) ; ucyeu(1:ndkx) = ucy(1:ndkx)
   if (jawave > 0 .and. .not. flowWithoutWaves) then
      do LL = 1,lnx
         Lb = Lbot(LL) ; Lt = Lb - 1 + kmxL(LL)
         do L = Lb, Lt
            if (ustokes(L) .ne. 0d0) then                    ! link flows
               k1 = ln(1,L)
               k2 = ln(2,L)
               ucxeu(k1) = ucxeu(k1) - wcx1(LL)*ustokes(L)
               ucyeu(k1) = ucyeu(k1) - wcy1(LL)*ustokes(L)
               ucxeu(k2) = ucxeu(k2) - wcx2(LL)*ustokes(L)
               ucyeu(k2) = ucyeu(k2) - wcy2(LL)*ustokes(L)
            endif
         enddo
      enddo
   endif
end subroutine getucxucyeuler
