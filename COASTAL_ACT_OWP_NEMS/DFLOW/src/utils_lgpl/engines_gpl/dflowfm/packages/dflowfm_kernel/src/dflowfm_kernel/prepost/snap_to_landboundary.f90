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

! $Id: snap_to_landboundary.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/snap_to_landboundary.f90 $

!> snap netnodes to land boundary segment
subroutine snap_to_landboundary()
   use m_netw
   use m_landboundary

   implicit none

   double precision :: xn, yn, ddis, rL

   integer          :: k, numlanseg, jstart, jend, j, MXLAN_sav

!  save MXLAN
   MXLAN_sav = MXLAN

!  set MXLAN to actual value
   MXLAN = MXLAN_loc

!   if ( jasnap.ne.2 .and. jasnap.ne.3 ) return

   do k=1,numk
      if ( nb(k).eq.1 .or. nb(k).eq.2 .or. nb(k).eq.3 ) then
         numlanseg = lanseg_map(k)
         if ( numlanseg.lt.1 ) cycle
         jstart    = lanseg_startend(1,numlanseg)
         jend      = lanseg_startend(2,numlanseg)
         call toland(xk(k),yk(k),jstart,jend,0,xn,yn,ddis,j,rL)
         xk(k) = xn
         yk(k) = yn
      end if
   end do

!  restore MXLAN
   MXLAN = MXLAN_sav

   return
end subroutine snap_to_landboundary
