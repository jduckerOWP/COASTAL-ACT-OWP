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

! $Id: renumber_cutcellmasks.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/renumber_cutcellmasks.f90 $

  subroutine renumber_cutcellmasks(perm)
     use network_data, only: numL
     use m_cutcells
     implicit none

     integer,          dimension(numL), intent(in)  :: perm !< permuation array

     integer,          dimension(:),    allocatable :: idxL_bak
     double precision, dimension(:),    allocatable :: xdxL_bak, ydxL_bak
     integer,          dimension(:),    allocatable :: pdxL_bak

     integer                                        :: i, ii, L, LL, num

     if ( jastored.ne.1 ) then
        return ! nothing to do
     end if

!    allocate
     allocate(idxL_bak(numL+1))
     num = idxL(numL+1)-1
     allocate(xdxL_bak(num))
     allocate(ydxL_bak(num))
     allocate(pdxL_bak(num))

!    copy
     do L=1,numL+1
        idxL_bak(L) = idxL(L)
     end do

     do i=1,num
        xdxL_bak(i) = xdxL(i)
        ydxL_bak(i) = ydxL(i)
        pdxL_bak(i) = pdxL(i)
     end do

!    apply permutation
     idxL_bak(1) = 1
     do LL=1,numL
        L = perm(LL)
        num = idxL_bak(L+1)-idxL_bak(L)
        idxL(LL+1) = idxL(LL) + num
        ii = idxL_bak(L)
        do i=idxL(LL),idxL(LL+1)-1
           xdxL(i) = xdxL_bak(ii)
           ydxL(i) = ydxL_bak(ii)
           pdxL(i) = pdxL_bak(ii)
           ii = ii+1
        end do
     end do

!    deallocate
     if ( allocated(idxL_bak) ) deallocate(idxL_bak)
     if ( allocated(xdxL_bak) ) deallocate(xdxL_bak)
     if ( allocated(ydxL_bak) ) deallocate(ydxL_bak)
     if ( allocated(pdxL_bak) ) deallocate(pdxL_bak)

     return
  end subroutine renumber_cutcellmasks
