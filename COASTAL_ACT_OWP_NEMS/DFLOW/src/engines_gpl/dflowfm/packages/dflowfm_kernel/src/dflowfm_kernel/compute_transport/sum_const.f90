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

! $Id: sum_const.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_transport/sum_const.f90 $

subroutine sum_const(iter, vol1)
   use m_transport
   use m_flowgeom, only: Ndx
   use m_flow, only: Ndkx
   implicit none

   integer,                           intent(in) :: iter
   double precision, dimension(Ndkx), intent(in) :: vol1

   double precision, dimension(NUMCONST)         :: sum

   integer                                       :: kk, k, kb, kt
   integer                                       :: j

   sum = 0d0

   do kk=1,Ndx
      call getkbotktop(kk,kb,kt)
      do k=kb,kt
         do j=1,NUMCONST
            sum(j) = sum(j) + vol1(k)*constituents(j,k)
         end do
      end do
   end do

   write(6,"(I5, ':', $)") iter
   do j=1,NUMCONST
      write(6,"(E25.15, $)") sum(j)
   end do
   write(6,*)


   return
end subroutine sum_const
