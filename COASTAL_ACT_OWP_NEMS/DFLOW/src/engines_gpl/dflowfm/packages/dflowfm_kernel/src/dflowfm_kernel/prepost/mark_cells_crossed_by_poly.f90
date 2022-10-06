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

! $Id: mark_cells_crossed_by_poly.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/mark_cells_crossed_by_poly.f90 $

!> mark the cells that are crossed by the polygon
   subroutine mark_cells_crossed_by_poly(ksize,kmask)
      use m_netw

      implicit none

      integer,                    intent(in) :: ksize  !< size of kmask array

      integer, dimension(ksize),  intent(in) :: kmask  !< original node mask, with new nodes set to 1

      integer, allocatable, dimension(:)     :: Lmask

      integer                                :: k, kk, k1, k2, L, N, lnn_orig

      logical                                :: Lcrossedcell

   !  allocate node mask arrays
      if ( allocated(cellmask) ) deallocate(cellmask)
      allocate(Lmask(numL),cellmask(nump))

   !  make the linkmask
      Lmask = 0
      do L=1,numL
         k1 = kn(1,L)
         k2 = kn(2,L)
         if ( k1.lt.1 .or. k2.lt.1 .or. k1.gt.numk .or. k2.gt.numk) cycle
         if ( kmask(k1).ne.kmask(k2) ) then
            Lmask(L) = 1
         else
            Lmask(L) = 0
         end if
      end do

   !  make the cellmask
      cellmask = 0
      do k=1,nump
         N = netcell(k)%N
         do kk=1,N
            L = netcell(k)%lin(kk)
            if ( Lmask(L).eq.1 ) then
               cellmask(k) = 1
               exit
            end if
         end do
      end do

   !  set lnn to 0 (deactivated, not a member of a crossed cell), 1 (member of one crossed cell) or 2 (member of two adjacent crossed cells)
      do L=1,numL
         lnn_orig = lnn(L)
         if ( lnn_orig.ge.1 ) then
            k1 = lne(1,L)
            if ( cellmask(k1).eq.1 ) then
               lnn(L) = 1
               lne(1,L) = k1
            else
               lnn(L) = 0
            end if
            if ( lnn_orig.eq.2 ) then
               k2 = lne(2,L)
               if ( cellmask(k2).eq.1 ) then
                  lnn(L) = lnn(L)+1
                  lne(lnn(L),L) = k2
               else
                  continue
               end if
            end if
         end if
      end do

   !  deallocate
      deallocate(Lmask, cellmask)

      return

   end subroutine mark_cells_crossed_by_poly
