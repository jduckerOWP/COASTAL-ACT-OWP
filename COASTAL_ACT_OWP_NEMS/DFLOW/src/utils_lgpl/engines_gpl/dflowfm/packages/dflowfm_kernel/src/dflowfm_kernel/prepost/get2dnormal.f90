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

! $Id: get2dnormal.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/get2dnormal.f90 $

 subroutine get2Dnormal(n1,xn1,yn1)  ! get x and y components of land normal vector pointing upward.

 use m_flowgeom                      ! = ok for all internal cells
 use m_flow
 use m_netw
 use m_sferic, only: jsferic, jasfer3D
 use m_missing, only : dxymis
 use geometry_module, only: normalin

 implicit none
 integer          :: n1,   k, L, LL, k3, k4
 double precision :: xn1, yn1, a, aa, alf, xt, yt, slope

 xn1 = 0d0 ; yn1 = 0d0; a = 0d0
 do k = 1, size(nd(n1)%ln)
    LL  = nd(n1)%ln(k)
    L   = iabs(LL)
    if (LL < 0) then ! incoming link has positive LL
       alf = acL(L)
    else
       alf = 1d0 - acL(L)
    endif
    aa = alf*wu(L)*dx(L) ; a  = a + aa
    k3 = lncn(1,L)       ; k4 = lncn(2,L)
    call normalin(xk(k3), yk(k3), xk(k4), yk(k4), xt, yt, xu(L), yu(L),jsferic, jasfer3D, dxymis)
    slope = ( zk(k4) - zk(k3) ) / wu(L)
    xn1   = xn1 + aa*xt*slope
    yn1   = yn1 + aa*yt*slope
 enddo
 if (aa > 0d0) then
    xn1 = xn1 / aa
    yn1 = yn1 / aa
 endif
 end subroutine get2Dnormal
