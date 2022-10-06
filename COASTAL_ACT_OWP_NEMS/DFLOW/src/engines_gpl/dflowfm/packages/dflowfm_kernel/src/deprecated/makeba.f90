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

! $Id: makeba.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/deprecated/makeba.f90 $

 subroutine makeba()  ! recompute ba
 use m_flow
 use m_flowgeom
 use m_netw
 implicit none
 integer          :: k,L,m,n,k1,k2
 double precision :: dxw, aa1

 ba = 0d0   ! ; acl = 0.5d0
 Do L = 1,lnx
    k1 = ln(1,L) ; k2 = ln(2,L)
    dxw    = 0.5d0*dx(L)*wu(L)
    ba(k1) = ba(k1) + dxw*acl(L)
    ba(k2) = ba(k2) + dxw*(1d0-acl(L))
 enddo

 do n   = 1, mxwalls
    k1  = walls(1,n)
    aa1 = walls(17,n)
    ba(k1) = ba(k1) + aa1
 enddo

 do L = Lnxi, Lnx
    k1 = ln(1,L) ; k2 = ln(2,L)
    ba(k1) = ba(k2)
 enddo

 end subroutine makeba
