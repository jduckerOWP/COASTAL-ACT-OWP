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

! $Id: showbitmap.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/showbitmap.f90 $

      SUBROUTINE SHOWBITMAP(jainterpolate)
      USE M_WEARELT
      USE M_BITMAP
      implicit none
      integer :: i
      integer :: ini
      integer :: j
      integer :: k
      integer :: key
      integer :: ndraw
      integer :: nko
      double precision :: xd
      double precision :: xs
      double precision :: xx
      double precision :: xx2
      double precision :: yd
      double precision :: ys
      double precision :: yy
      double precision :: yy2
      double precision :: zs
      integer :: jainterpolate
      COMMON /DRAWTHIS/  ndraw(50)


      CALL IGRCOLOURMODEL(24)

      INI = 1
      XX  = 2
      YY  = 2
      CALL BILINXY(XB, YB, XP, YP, XX, YY, XX2, YY2, INI)
      IF (INI .EQ. -1) RETURN
      INI = 0

      XD  = (XP(2)-XP(1))/(XB(2)-XB(1))
      YD  = (YP(3)-YP(1))/(YB(3)-YB(1))
      XD  = XD/2
      YD  = YD/2

      DO J = NXP,1,-1
         CALL HALT2(KEY)
         IF (KEY .EQ. 1) THEN
            CALL IGRCOLOURMODEL(8)
            RETURN
         ENDIF
         NKO    = -1
         DO I   = 1,MXP
            K   = (NXP-J)*MXP + I
            XX  = dble(I-1)
            YY  = dble(J-1)
            CALL BILINXY(XB, YB, XP, YP, XX, YY, XX2, YY2, INI)

            if (jainterpolate==1) then
               xs = xx2
               ys = yy2
               zs = 1e-6*ipix(k)
               call pixcount(xs,ys,zs,1)
            endif

            IF (XX2 .GT. X1 .AND. XX2 .LT. X2 .AND. YY2 .GT. Y1 .AND. YY2 .LT. Y2 ) THEN
                IF (NKO .NE. IPIX(K)) THEN
                   CALL SETCOL(IPIX(K))
                   NKO = IPIX(K)
                ENDIF
                IF (NDRAW(10) .EQ. 0) THEN
                   call RECTANGLE(real(XX2-XD),real(YY2-YD),real(XX2+XD),real(YY2+YD))
                 ! CALL IGRMOVETO(XX2-XD,YY2-YD)
                 ! CALL IGrRECTANGLEREL(XD*2,YD*2)
                ELSE
                   CALL KREC5(XX2,YY2,XD,YD)
                ENDIF
            ENDIF
         ENDDO
      ENDDO
      CALL IGRCOLOURMODEL(8)

      if (jainterpolate==1) then
         call pixcount(xs,ys,zs,2)
      endif

      RETURN
      END
