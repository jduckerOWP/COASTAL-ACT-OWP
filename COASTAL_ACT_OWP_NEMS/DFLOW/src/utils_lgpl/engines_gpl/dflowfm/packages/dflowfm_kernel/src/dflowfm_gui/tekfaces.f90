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

! $Id: tekfaces.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/tekfaces.f90 $

      SUBROUTINE TEKFACES()

      use unstruc_colors
      use m_netw
      use sorting_algorithms, only: indexx
      use gridoperations

      implicit none
      integer :: ierr
      integer :: k
      integer :: l
      integer :: n
      integer :: ncol
      integer :: ni

      DOUBLE PRECISION   XX,YY,ZZ, XH(10), YH(10), ZH(10)
      INTEGER, ALLOCATABLE, SAVE        :: NP(:)
      double precision :: XP, YP
      double precision, ALLOCATABLE, SAVE        :: ZP(:)

      IF (SIZE(NP) .LT. NUMP) THEN
         IF ( ALLOCATED(NP) ) DEALLOCATE(NP,ZP)
         ALLOCATE (NP(NUMP),ZP(NUMP),STAT = IERR)
      ENDIF

      IF (NUMP .NE. 0) THEN
         DO N  = 1, NUMP
            XX = 0
            YY = 0
            ZZ = 0
            DO K = 1,netcell(N)%N
               XX = XX + XK(netcell(N)%NOD(K))
               YY = YY + YK(netcell(N)%NOD(K))
               ZZ = ZZ + ZK(netcell(N)%NOD(K))
            ENDDO
            XX = XX/netcell(N)%N
            YY = YY/netcell(N)%N
            ZZ = ZZ/netcell(N)%N
            CALL DRIETWEE(XX,YY,ZZ,XP,YP,ZP(N))
         ENDDO
         call indexx(NUMP,ZP,NP)

         DO L  = NUMP, 1, -1
            N  = NP(L)
            NI = netcell(N)%N
            DO K = 1, NI
               XH(K) = XK(netcell(N)%NOD(K))
               YH(K) = YK(netcell(N)%NOD(K))
               ZH(K) = ZK(netcell(N)%NOD(K))
            ENDDO
            IF (NI .EQ. 6) THEN
               NCOL = 221
            ENDIF
            IF (NI .EQ. 5) THEN
               NCOL = 111
            ENDIF
            IF (NI .EQ. 4) THEN
               NCOL =  31
            ENDIF
            IF (NI .EQ. 3) THEN
               NCOL = 171
            ENDIF
            CALL PFILLER (XH,YH,NI,NCOL,NCOLLN)
         ENDDO
      ENDIF

      RETURN
      END SUBROUTINE TEKFACES
