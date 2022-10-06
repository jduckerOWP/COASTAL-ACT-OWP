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

! $Id: comp_horfluxtot.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_transport/comp_horfluxtot.f90 $

subroutine comp_horfluxtot()
   use m_flowgeom, only: Lnx
   use m_flow, only: Lbot, Ltop, kmx, Lnkx
   use m_transport, only: ISED1, ISEDN, fluxhor, fluxhortot, sinksetot, sinkftot
   use m_flowtimes, only: dts
   use timers

   implicit none

   integer :: LL, L, Lb, Lt
   integer :: j

   integer(4) ithndl /0/
   if (timon) call timstrt ( "comp_horfluxtot", ithndl )

   if ( kmx<1 ) then
      do L=1,Lnx
         do j=ISED1, ISEDN
            fluxhortot(j,L) = fluxhortot(j,L) + fluxhor(j,L) * dts
         end do
      end do
   else
      do LL=1,Lnx
         Lb = Lbot(LL)
         Lt = Ltop(LL)
         do L=Lb,Lt
            do j=ISED1, ISEDN
               fluxhortot(j,L) = fluxhortot(j,L) + fluxhor(j,L) * dts
            end do
         end do
      end do
   end if

   if (timon) call timstop( ithndl )
end subroutine comp_horfluxtot
