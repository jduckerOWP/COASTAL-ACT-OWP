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

! $Id: zerolan.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/rest_f90/zerolan.f90 $

      SUBROUTINE ZEROLAN( KEY)
      use m_landboundary
      use m_polygon
      use m_missing
      use geometry_module, only: dbpinpol
      implicit none
      integer :: i
      integer :: inhul
      integer :: istart
      integer :: ja
      integer :: k
      integer :: key
      integer :: mxol
      integer :: ntot
      KEY = 3
      IF (NPL .LE. 2) THEN
         CALL CONFRM('NO POLYON, SO DELETE all BOUNDARY POINTS ? ',JA)
         IF (JA .EQ. 0) THEN
            KEY = 0
            RETURN
         ENDIF
!        CALL SAVESAM()
         DO 5 I = 1,MXLAN
            XLAN(I)  = dmiss
            YLAN(I)  = dmiss
            ZLAN(I)  = dmiss
            NCLAN(I) = 0
    5    CONTINUE
         MXLAN = 0
         RETURN
      ENDIF
!     CALL SAVESAM()
      INHUL = -1
      DO 10 I = 1,MXLAN
            CALL DBPINPOL( XLAN(I), YLAN(I), INHUL, dmiss, JINS, NPL, xpl, ypl, zpl)
            IF (INHUL .EQ. 1) THEN
               XLAN(I)  = dmiss
               YLAN(I)  = dmiss
               ZLAN(I)  = dmiss
               NCLAN(I) = 0
            ENDIF
   10 CONTINUE

      K = 0
      MXOL   = MXLAN
      ISTART = 0
      NTOT   = 0
      DO 20 I = 1,MXLAN
         IF (XLAN(I) .NE. dmiss) THEN
            ISTART   = 1
            K        = K + 1
            XLAN(K)  = XLAN(I)
            YLAN(K)  = YLAN(I)
            ZLAN(K)  = ZLAN(I)
            NCLAN(K) = NCLAN(I)
         ELSE IF (ISTART .EQ. 1) THEN
            K       = K + 1
            XLAN(K)  = dmiss
            YLAN(K)  = dmiss
            ZLAN(K)  = dmiss
            NCLAN(K) = 0
            ISTART   = 0
         ENDIF
   20 CONTINUE
      MXLAN = K

      DO 30 I = MXLAN+1,MXOL
         XLAN(I)  = dmiss
         YLAN(I)  = dmiss
         ZLAN(I)  = dmiss
         NCLAN(I) = 0
   30 CONTINUE

      RETURN
      END
