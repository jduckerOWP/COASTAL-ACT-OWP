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

! $Id: cosphiunet.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/cosphiunet.f90 $

 double precision function cosphiunet(L)                ! get link cos on net link

 use m_flowgeom
 use m_netw
 use geometry_module, only: dcosphi
 use m_sferic, only: jsferic, jasfer3D
 use m_missing, only : dxymis

 implicit none


 integer :: L                                        ! for net link L,

 ! locals
 integer          :: k1, k2, k3, k4
 double precision :: rn, rt, rnl, rtl

 ! Check: no findcells done yet. Report 'all bad'.
 if (nump <= 0) then
    cosphiunet = 1
    return
 end if

 ! Check: 1D or closed boundary link: report 'good'.
 if (lnn(L) < 2) then
    cosphiunet = 0
    return
 elseif (lne(1,L) <= 0 .or. lne(2,L) <= 0) then
    cosphiunet = 0
    return
 elseif (kn(1,L) <= 0 .or. kn(2,L) <= 0) then
    cosphiunet = 0
    return
 end if

 k1 = lne(1,L)
 k2 = lne(2,L)
 k3 = kn(1,L)
 k4 = kn(2,L)
 cosphiunet = dcosphi(xz(k1), yz(k1), xz(k2), yz(k2), xk(k3), yk(k3), xk(k4), yk(k4), jsferic, jasfer3D, dxymis)

 end function cosphiunet
