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

! $Id: getfetch.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_waves/getfetch.f90 $

subroutine getfetch(k,U10,FetchL,FetchD) !and windspeed
use m_flow,  only:  Hs, Wx, Wy
use m_waves, only:  fetch, nwf, fetdp
use m_sferic
implicit none

integer          :: k
double precision :: U10,FetchL,FetchD

integer          :: L,nw1,nw2
double precision :: alfa1, alfa2, dir


FetchL = 0d0 ; FetchD = 0d0

if (Hs(k) > 0d0) then
   call getlink1(k,L) ! het is maar voor wind
   U10 = sqrt ( WX(L)*WX(L) + WY(L)*WY(L) )
   IF (U10 .LT. 1d0) return

   DIR   = ATAN2(WY(L), WX(L))
   IF (DIR < 0D0) DIR = DIR + TWOPI

   dir = dir/twopi
   if (dir >= 1d0) dir = 0d0
   NW1 = DIR*(nwf-1) + 1
   NW2 = NW1 + 1

   if (fetch(nw1,k) > 0d0 .or. fetch(nw2,k) > 0d0 ) then
      alfa2  = (nwf-1)*( dir - dble(nw1-1) / dble(nwf-1) )
      alfa1  = 1d0 - alfa2
      fetchL = alfa1*fetch(nw1,k) + alfa2*fetch(nw2,k)
      fetchD = alfa1*fetdp(nw1,k) + alfa2*fetdp(nw2,k)
   endif
endif
end subroutine getfetch
