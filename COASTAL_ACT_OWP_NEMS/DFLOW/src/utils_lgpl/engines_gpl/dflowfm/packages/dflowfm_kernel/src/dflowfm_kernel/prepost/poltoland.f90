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

! $Id: poltoland.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/poltoland.f90 $

      SUBROUTINE POLTOLAND(L1,L2)               ! SHIFT POLYGON TO LANDBOUNDARY
      USE M_POLYGON
      USE M_MISSING
      USE M_LANDBOUNDARY
      implicit none
      integer :: l1
      integer :: l2

      integer :: in, jn
      integer :: l, j
      double precision :: xp, yp, xpn, ypn, dis, rL

      IN = 1 ; IF (L2 < L1) IN = -1
      DO L = L1,L2, IN
         XP = XPL(L)
         IF (XP .NE. XYMIS) THEN
            YP = YPL(L)
            CALL TOLAND(XP,YP, 1, MXLAN, 1, xpn, ypn, dis, j, rL)
            XPL(L) = xpn ; YPL(L) = ypn
         ENDIF
      ENDDO

      END SUBROUTINE POLTOLAND
