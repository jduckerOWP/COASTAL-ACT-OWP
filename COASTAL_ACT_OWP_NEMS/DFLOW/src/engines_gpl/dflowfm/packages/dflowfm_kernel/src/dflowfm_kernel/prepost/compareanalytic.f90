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

! $Id: compareanalytic.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/compareanalytic.f90 $

subroutine compareanalytic(s,u,x,mmax)

use m_flowgeom
use m_flow

implicit none
integer :: mmax
double precision :: s(0:mmax),u(0:mmax),x(0:mmax)
double precision :: alf, dif, si, aa
integer          :: n, i, ii
logical inview

call statisticsnewstep()

call setcol(221)
do n = 1,ndx

   if (.not. inview( xz(n), yz(n) ) ) cycle

   i = 0
   do ii = 1, mmax-1
      if ( x(ii) <= xz(n) .and. xz(n) < x(ii+1) ) then
         i = ii
         exit
      endif
   enddo
   !i = (xz(n) + 0.5*dxw) / dxw
   if  ( i > 2 .and. i < mmax-1 ) then
       alf = (xz(n) - x(i) ) / ( x(i+1) - x(i) )
       if (alf < 0d0 .or. alf > 1d0) then
           si  = 0
       else
           si  = (1-alf)*s(i) + alf*s(i+1)
           dif = abs(s1(n) - si)
           call statisticsonemorepoint(dif)
        !   call ptabs(xz(n), bl(n) + 100d0*dif)
       endif
   endif
enddo
call statisticsfinalise()

end subroutine compareanalytic
