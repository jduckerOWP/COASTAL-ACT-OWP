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

! $Id: tekspw.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/tekspw.f90 $

 subroutine tekspw(vfac,met)
 use m_flowgeom
 use m_spiderweb
 use m_wind
 implicit none
 double precision :: vfac, shft
 integer          :: met

 integer          :: mx, nx, i, j, L

 shft  = 0d0
 mx    = size(spw,2)
 nx    = size(spw,3)
 if (sum(xu(:)) .lt. 0) then
     shft       = 1d0
 end if
 if (mx.ne.0 .and. nx.ne.0) then
    do i = 1,mx-1
       do j = 1,nx
          call setcol(221)
          call arrowsxy( spw(1,i,j) - shft*360d0, spw(2,i,j), spw(3,i,j) , spw(4,i,j), 0.05*VFAC)
       enddo
    enddo
 endif
 if (allocated(wx)) then
    do L  = 1,lnxi
       call setcol(224)
       call arrowsxy( xu(L) , yu(L) , wx(L) , wy(L), 0.05*VFAC)
    enddo
 endif

 end subroutine tekspw
