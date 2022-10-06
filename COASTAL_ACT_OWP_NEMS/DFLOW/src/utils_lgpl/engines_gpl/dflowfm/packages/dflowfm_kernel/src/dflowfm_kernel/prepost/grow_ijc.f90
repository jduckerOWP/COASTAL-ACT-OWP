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

! $Id: grow_ijc.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/grow_ijc.f90 $

!> grow ijc with blocksize to satisfy objective lower- and upperbound
subroutine grow_ijc(lowold, uppold, lowobj, uppobj, init)

 use m_alloc
 use m_grid
 use m_missing

 implicit none

 integer, dimension(2), intent(inout) :: lowold, uppold    !< current array sizes
 integer, dimension(2), intent(in)    :: lowobj, uppobj    !< objective array sizes

 integer                              :: init              !< init=1: set blocksizes to initial values

 integer, dimension(2)                :: lownew, uppnew
 integer                              :: i
! integer, parameter                   :: IMISS = -999999
 integer, parameter                   :: IJCBLOCK = 100    ! block size in ijc

 logical                              :: ldoit

 integer, dimension(2), save          :: blocklow          ! lower blocksizes in ijc
 integer, dimension(2), save          :: blockupp          ! upper blocksizes in ijc
 double precision, parameter          :: FAC = 1.2         ! growfactor of blocksizes

 if ( init.eq. 1 ) then
    blocklow = (/ 1, 1 /)
    blockupp = (/ 1, 1 /)
 end if

 lownew = lowold
 uppnew = uppold

 if ( (lownew(1) .gt. lowobj(1)) .or. (uppnew(1) .lt. uppobj(1)) .or. &
      (lownew(2) .gt. lowobj(2)) .or. (uppnew(2) .lt. uppobj(2)) ) then

    do i=1,2
       do while ( lownew(i) .gt. lowobj(i) )
          lownew(i) = lownew(i) - blocklow(i)
       end do

       do while ( uppnew(i) .lt. uppobj(i) )
          uppnew(i) = uppnew(i) + blockupp(i)
       end do
    end do

    do i=1,2
       if ( lownew(i).ne.lowold(i) )   &
          blocklow(i) = ceiling( dble(blocklow(i)) * FAC )
       if ( uppnew(i).ne.uppold(i) )   &
          blockupp(i) = ceiling( dble(blockupp(i)) * FAC )
    end do

    call realloc(ijc, uppnew, lownew, fill=IMISS)

    lowold = lownew
    uppold = uppnew
 end if

end subroutine grow_ijc
