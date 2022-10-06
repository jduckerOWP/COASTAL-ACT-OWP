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

! $Id: isnode.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/isnode.f90 $

  SUBROUTINE ISNODE(KP, XP, YP, ZP)

  use m_netw
  use m_wearelt
  use m_missing
  use m_sferic
  use m_sferzoom

  implicit none

  integer :: KP
  double precision :: XP, YP, ZP

  integer :: ll
  double precision :: xkk, ykk, zkk, rcx,rcy,dis
  integer :: K, KPREV

  IF (KP < 0) THEN
     KPREV = IABS(KP)
  ELSE
     KPREV = 0
  ENDIF

  if (jsfertek > 0) then
       rcy = cr*dyh*ra*dg2rd
     ! call setrcirxy(xp,yp,rcx,rcy)
  endif

  KP = 0
  ZP = dmiss
  DO K = 1,NUMK
     if (jsfertek > 0) then
        call dbdistancehk(xk(k),yk(k),xp,yp,dis)
        if (dis< rcy) then
           kp = k
        endif
     else
        IF (ABS(XK(k)-XP) .LT. rcir .AND. ABS(YK(k)-YP) .LT. rcir) THEN
            KP = K
        endif
     endif
     if (kp > 0) then
         CALL DISPNODE(KP)
         ZP  = ZK(kp)
!         XYZ = ZKK
         RETURN
     ENDIF
  ENDDO

  END SUBROUTINE ISNODE
