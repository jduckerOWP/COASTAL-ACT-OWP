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

! $Id: disable_invalid_ghostcells_with_wu.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/disable_invalid_ghostcells_with_wu.f90 $

!> disable ghostcells that have no internal cells in the other subdomains by setting the wu of their flowlinks to zero
!>   and rely on sethu to disable the flowlinks
!>   an invalid ghostcell is a flownode, say k, that:
!>     -is not a boundary node (k.le.Ndxi), and
!>     -is not in the own subdomain (idomain(k).ne.my_rank), and
!>     -is not a member of ghostlist_sall
subroutine disable_invalid_ghostcells_with_wu()
   use m_partitioninfo
   use m_flowgeom, only: Ndx, Ndxi, nd, wu
   implicit none

   integer, dimension(:), allocatable          :: imask
   integer                                     :: i, k, L
   integer                                     :: ierror

   ierror = 0  ! so far, so good
   if ( jampi.eq.0 ) return   ! nothing to do

   ierror = 1

!  allocate
   allocate(imask(Ndx)) ! safety, could also be Ndxi

!  mark the flownodes in the ghostlist
   imask = 0
   do i=1,nghostlist_sall(ndomains-1)
      k = ighostlist_sall(i)
      imask(k) = 1
   end do

!  check non-boundary flownodes and disable cells that are neither in own subdomain nor in ghostlist by setting wu's of their flowlinks to zero
   do k=1,Ndxi
      if ( imask(k).eq.0 .and. idomain(k).ne.my_rank ) then
         do i=1,nd(k)%lnx
            L = iabs(nd(k)%ln(i))
            wu(L) = 0d0
         end do
      end if
   end do

   ierror = 0
1234 continue

   if ( allocated(imask) ) deallocate(imask)

   return
   end subroutine disable_invalid_ghostcells_with_wu
