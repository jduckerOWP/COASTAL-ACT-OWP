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

! $Id: doforester.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_transport/doforester.f90 $

subroutine doforester()
use m_flow    ,   only : sa1, vol1, ndkx, kbot, ktop, kmxn, ndkx, maxitverticalforestersal, maxitverticalforestertem
use m_flowgeom,   only : ndx, ndxi
use m_turbulence, only : kmxx
use m_transportdata ! ,  only : constituents, numconst, itemp
use timers

implicit none

integer          :: kk, km, kb
double precision :: a(kmxx), d(kmxx)

integer(4) ithndl /0/
if (timon) call timstrt ( "doforester", ithndl )

do kk = 1,ndxi
   kb = kbot(kk)
   km = ktop(kk) - kb + 1
   if (maxitverticalforestersal > 0) then
      call foresterpoint2(constituents, numconst, ndkx, isalt, vol1(kb:), a, d, km, kmxn(kk), kb, maxitverticalforestersal,  1)
   endif
   if (maxitverticalforestertem > 0) then
      call foresterpoint2(constituents, numconst, ndkx, itemp, vol1(kb:), a, d, km, kmxn(kk), kb, maxitverticalforestertem, -1)
   endif
enddo

if (timon) call timstop( ithndl )
end subroutine doforester
