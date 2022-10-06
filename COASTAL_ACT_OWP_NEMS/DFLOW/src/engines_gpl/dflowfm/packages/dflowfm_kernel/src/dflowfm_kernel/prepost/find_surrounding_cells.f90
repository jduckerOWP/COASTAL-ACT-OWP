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

! $Id: find_surrounding_cells.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/find_surrounding_cells.f90 $

!>  find cells that are directly and indirectly connected to cell k
   subroutine find_surrounding_cells(kcell, nmax, ndirect, nindirect, kdirect, kindirect, kne)
      use m_netw

      implicit none

      integer,                  intent(in)    :: kcell      !< cell number
      integer,                  intent(in)    :: nmax       !< array size
      integer,                  intent(out)   :: ndirect    !< number of   directly connected cells (=netcell()%N)
      integer,                  intent(out)   :: nindirect  !< number of indirectly connected cells
      integer, dimension(nmax), intent(out)   :: kdirect    !< directly connected cells
      integer, dimension(nmax), intent(out)   :: kindirect  !< indirectly connected cells
      integer, dimension(2,nmax), intent(out) :: kne        !< two indirectly connected cells that are adjacent to a directly connected cell

      integer                               :: nneighbors

      integer                              :: i, j, kk, kkk, kkkk, kcell1, kcell2, k1, L

      ndirect   = 0
      kdirect   = 0
      nindirect = 0
      kindirect = 0
      kne       = 0

!     find the directly connected cells
 kklp:do kk=1,netcell(kcell)%N
         L = netcell(kcell)%lin(kk)

         if ( lnn(L).lt.2 ) cycle

         kcell2 = lne(1,L) + lne(2,L) - kcell   ! other cell

!        check and see if the cell is already administered
         do kkk=1,ndirect
            if ( kcell2.eq.kdirect(kkk) ) cycle kklp
         end do

         ndirect = ndirect+1
         if ( ndirect.gt.nmax ) then
            ndirect = ndirect-1
            call qnerror('find_surrounding_cells: array too small', ' ', ' ')
            return
         end if
         kdirect(ndirect) = kcell2
      end do kklp

!     find the cells indirectly connected cells
      do kk=1,netcell(kcell)%N
         k1 = netcell(kcell)%nod(kk)
         do kkk=1,nmk(k1)
            L = nod(k1)%lin(kkk)
        ilp:do i=1,lnn(L)
               kcell2 = lne(i,L)
!              check and see if the cell is new
               if ( kcell2.eq.kcell ) cycle ilp

               do kkkk=1,ndirect
                  if ( kcell2.eq.kdirect(kkkk) ) cycle ilp
               end do

               do kkkk=1,nindirect
                  if ( kcell2.eq.kindirect(kkkk) ) cycle ilp
               end do

!              add new cell
               nindirect = nindirect + 1
               if ( nindirect.gt.nmax ) then
                  nindirect = nindirect-1
                  call qnerror('find_surrounding_cells: array size too small', ' ', ' ')
                  return
               end if
               kindirect(nindirect) = kcell2
            end do ilp
         end do
      end do


!     find the adjacent cells
      do i=1,ndirect
         kcell1 = kdirect(i)
         do j=1,netcell(kcell1)%N
            L = netcell(kcell1)%lin(j)
            if ( lnn(L).lt.2 ) cycle
            kcell2 = lne(1,L) + lne(2,L) - kcell1
!           check and see if this cell is administered
            do kk=1,ndirect
               if ( kdirect(kk).eq.kcell2 ) then
                  if ( kne(1,i).eq.0 ) then
                     kne(1,i) = -kcell2
                     kcell2 = -1234
                  else
                     kne(2,i) = -kcell2
                     kcell2 = -1234
                  end if
               end if
            end do

            if ( kcell2.eq.-1234 ) cycle

            do kk=1,nindirect
               if ( kindirect(kk).eq.kcell2 ) then
                  if ( kne(1,i).eq.0 ) then
                     kne(1,i) = kcell2
                  else
                     kne(2,i) = kcell2
                  end if
               end if
            end do

         end do
      end do

      return
   end subroutine find_surrounding_cells
