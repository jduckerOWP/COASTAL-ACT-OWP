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

! $Id: removecell.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/removecell.f90 $

!  remove a netcell
   subroutine removecell(xp,yp)
      use m_netw
      use m_missing, only: jins, dmiss
      use geometry_module, only: pinpok
      use gridoperations

      implicit none

      integer, save                               :: NEEDFINDCELLS=1

      double precision,                intent(in) :: xp, yp             !< coordinates of input point

      integer                                     :: k, in

      if ( nump.lt.1 ) NEEDFINDCELLS=1

      if ( NEEDFINDCELLS.ne.0 .or. netstat.ne.NETSTAT_OK ) then
         call findcells(100)
         call makenetnodescoding()
         NEEDFINDCELLS = 0
      end if

!     (re)allocate
      if ( allocated(cellmask) ) deallocate(cellmask)
      allocate(cellmask(nump))
      cellmask = 0

      !  find the cell
      in = 0
      do k = 1,nump
         if ( netcell(k)%N.lt.1 ) cycle
         call pinpok(xp, yp, netcell(k)%N, xk(netcell(k)%nod), yk(netcell(k)%nod), in, jins, dmiss)
         if ( in.gt.0 ) exit
      end do

      if ( in.eq.0 ) then  ! no cell found
         call qnerror('removecell: no cell found', ' ', ' ')
         goto 1234
      end if

!     mask cell
      cellmask(k) = 1

!     remove masked cells
      call remove_masked_netcells()

 1234 continue

!     deallocate
      deallocate(cellmask)

      return
   end subroutine removecell
