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

! $Id: bilinxy.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/rest_f90/bilinxy.f90 $

      SUBROUTINE BILINXY(X, Y, XZ, YZ, XP, YP, XP2, YP2, INI)
      implicit none
      double precision :: c
      integer :: i
      integer :: ini
      integer :: japarallel
      double precision, SAVE    :: A(4,4), BX(4), BY(4)
      double precision :: X(4), Y(4), XZ(4), YZ(4), XP, YP, XP2, YP2
      INTEGER, SAVE :: INX(4)
      ! (Zi = AXi + BYi + CXiYi + Di ,i=1,4)
      ! Coefficienten in A, rechterlid in B, opl met LU-decompositie
      IF (INI .EQ. 1) THEN
         DO I = 1,4
            A(I,1) =  X(I)-X(1)
            A(I,2) =  Y(I)-Y(1)
            A(I,3) = (Y(I)-Y(1))*(X(I)-X(1))
            A(I,4) =  1
            BX(I)  =  XZ(I)
            BY(I)  =  YZ(I)
         ENDDO
         CALL LUDCMP(A,4,4,INX,C,JAPARALLEL)
         IF (JAPARALLEL .EQ. 1) THEN
            CALL qnerror('Problem in Ludcmp',' ',' ')
            INI = -1
            RETURN
         ENDIF
         CALL LUBKSB(A,4,4,INX,BX)
         CALL LUBKSB(A,4,4,INX,BY)
      ENDIF
      XP2 = (XP-X(1))*BX(1) + (YP-Y(1))*BX(2) + (XP-X(1))*(YP-Y(1))*BX(3) + BX(4)
      YP2 = (XP-X(1))*BY(1) + (YP-Y(1))*BY(2) + (XP-X(1))*(YP-Y(1))*BY(3) + BY(4)
      RETURN
      END
