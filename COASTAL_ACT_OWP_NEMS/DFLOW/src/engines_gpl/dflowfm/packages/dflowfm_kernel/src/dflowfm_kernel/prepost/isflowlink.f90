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

! $Id: isflowlink.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/isflowlink.f90 $

SUBROUTINE ISflowlink(XP, YP, LL) ! IS THIS A flow NODE OR A flow LINK ?
 use m_netw
 use m_flowgeom
 use m_wearelt
 implicit none
 double precision :: XP, YP
 integer :: LL
 integer n, l, k1, k2
 double precision :: xa, ya

 LL = 0

 DO L = 1,lnx
    if (L > lnx1D) then
       k1 = lncn(1,l) ; k2 = lncn(2,L)  ! eigenlijk 3 en 4
       xa = 0.5*(xk(k1) + xk(k2)) ; ya = 0.5*(yk(k1) + yk(k2))
    else
       k1 = ln(1,L) ; k2 = ln(2,L)
       xa = 0.5*(xz(k1) + xz(k2)) ; ya = 0.5*(yz(k1) + yz(k2))
    endif

    IF (ABS(XA-XP) .LT. 3*RCIR .AND. ABS(YA-YP) .LT. 3*RCIR) THEN
        LL = L
        CALL DISLN(LL)
        RETURN
    ENDIF
 ENDDO

 RETURN
 END SUBROUTINE ISflowlink
