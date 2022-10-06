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

! $Id: islink.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/islink.f90 $

  !> Tries to find the number of the netlink close to a point.
  !! The provided point should lie within a rhombus with the netlink
  !! as a diagonal and another diagonal with length searchradius rcir.
  !! The returned zp value is the z-coordinate of the link's center.
  SUBROUTINE ISLINK(LL, XP, YP, ZP)

  use m_netw
  use m_wearelt
  use m_missing, only: jins, dmiss, dxymis
  use geometry_module, only: pinpok, normalout
  use m_sferic, only: jsferic, jasfer3D
  use gridoperations

  implicit none
  integer, intent(out)          :: LL     !< Number of first netlink found, 0 if none.
  double precision, intent(in)  :: XP, YP !< Coordinates of input point.
  double precision, intent(out) :: ZP     !< Z-coordinate of netlink's center.

  integer          :: jav, jview
  integer          :: k1, k2, l, in, jins_old
  double precision :: xkk
  double precision :: xyz
  double precision :: ykk
  double precision :: zkk
  double precision :: xprange(4), yprange(4)
  double precision :: xk1p, yk1p, xk2p, yk2p, rx, ry

  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
  DOUBLE PRECISION :: H=0.5d0

! store
  jins_old = jins
  jins = 1  ! otherwise pinpok has inverse behavior

  LL = 0
  DO L = 1,NUML
     K1  = KN(1,L) ; K2  = KN(2,L)
     IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
        CALL DRIETWEE(H*(XK(K1)+XK(K2)),H*(YK(K1)+YK(K2)),H*(ZK(K1)+ZK(K2)),XKK,YKK,ZKK)
        ! Get screen-projected coordinates of link nodes 1 and 2, and construct search range around it.
        CALL DRIETWEE(XK(K1), YK(K1), ZK(K1), xk1p,yk1p,ZKK)
        CALL DRIETWEE(XK(K2), YK(K2), ZK(K2), xk2p,yk2p,ZKK)
        call normalout(xk1p, yk1p, xk2p, yk2p, rx, ry, jsferic, jasfer3D, dmiss, dxymis)
        xprange(1) = xk1p;        yprange(1) = yk1p
        xprange(2) = xkk+rcir*rx; yprange(2) = ykk+rcir*ry
        xprange(3) = xk2p;        yprange(3) = yk2p
        xprange(4) = xkk-rcir*rx; yprange(4) = ykk-rcir*ry

!        call movabs(xprange(1), yprange(1))
!        call clnabs(xprange(2), yprange(2), 41)
!        call clnabs(xprange(3), yprange(3), 41)
!        call clnabs(xprange(4), yprange(4), 41)
        call PINPOK(xp, yp, 4, xprange, yprange, IN, jins, dmiss)
        if (in == 1) then
            LL = L
            CALL DISPNODE(LL)
            ZP  = ZKK
            return
        end if

!        IF (ABS(XKK-XP) .LT. RCIR .AND. ABS(YKK-YP) .LT. RCIR) THEN
!            LL = L
!            CALL DISPNODE(LL)
!            ZP  = ZKK
!            XYZ = ZKK
!            RETURN
!        ENDIF
     ENDIF
  ENDDO
  ZP = XYZ
  CALL DISPNODE(LL)

! restore
  jins = jins_old

  RETURN
  END SUBROUTINE ISLINK
