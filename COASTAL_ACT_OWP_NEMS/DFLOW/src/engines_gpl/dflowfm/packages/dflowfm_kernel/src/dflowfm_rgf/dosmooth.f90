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

! $Id: dosmooth.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_rgf/dosmooth.f90 $

      SUBROUTINE DOSMOOTH(NFLD)
      use m_gridsettings
      use m_grid
      use unstruc_colors
      use unstruc_messages
      implicit none
      integer :: nfld

      integer :: ndraw
      COMMON /DRAWTHIS/ ndraw(50)

      integer :: MB,NB,MB2,NB2,NPT,NPT2,NPUTO,ITYPE
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      double precision, allocatable :: XH(:,:), YH(:,:)

      integer :: MD, ND, M1, M2, N1, N2, JS, I, J, K, JA1, JA2
      double precision :: R1, R2, R3, FR, XX, YY, X21, X22, Y21, Y22,X41, X42, Y41, Y42, &
                          A, B, TV1, TV2

      allocate(XH(MMAX,NMAX), YH(MMAX,NMAX))

      IF (NDRAW(8) .EQ. 0) CALL READYY('SMOOTHING GRID', 0d0 )
!      CALL ISITU(      X,      Y,     MC,   NC,    IJC,  IJYES)!!!Oud
      ! Deze routine dosmooth wordt alleen uit editgridlineblock aangeroepen
      ! met de xc, ijyes, etc. uit m_grid. Diezelfde m_grid wordt in isitu gebruikt
      ! dus hoeven het niet meer door te geven.
      CALL ISITU()
      CALL PUTARR(Xc,XH,MMAX,NMAX)
      CALL PUTARR(Yc,YH,MMAX,NMAX)

      MD = MB(2) - MB(1)
      ND = NB(2) - NB(1)

      M1 = MB(3)
      N1 = NB(3)
      M2 = MB(4)
      N2 = NB(4)

      JS = 1
      DO 10 K = 1,ITSMO

         IF (MD .EQ. 0 .AND. NFLD .EQ. 9) THEN
!           verticale linemodes

            DO 20 I = 1,MC
               DO 20 J = 2,NC-1
                  IF (J .GT. N1 .AND. J .LT. N2) THEN
                     IF (IJC(I,J) .EQ. 10 .OR. IJC(I,J) .EQ.  2 .OR. IJC(I,J) .EQ.  4 ) THEN
                         R1 = SQRT( (XH(I,J) - XH(I,J-1))**2 +  &
                                    (YH(I,J) - YH(I,J-1))**2 )
                         R2 = SQRT( (XH(I,J) - XH(I,J+1))**2 +  &
                                    (YH(I,J) - YH(I,J+1))**2 )

                         IF ( (M2-M1) .NE. 0) THEN
                            CALL SMEERFUNCTIE(I,J,MB(1),J,FR,1,0)
                         ELSE
                            FR = 1d0
                         ENDIF

                         IF (JS .EQ. 1) THEN
                            IF (R1 .GT. R2) THEN
                               R3 = (R1 - R2)/2
                               A  = FR*CSMO*R3/R1
                               XX = XH(I,J) + A*( XH(I,J-1) - Xc(I,J) )
                               YY = YH(I,J) + A*( YH(I,J-1) - Yc(I,J) )
                            ELSE
                               R3 = (R2 - R1)/2
                               A  = FR*CSMO*R3/R2
                               XX = XH(I,J) + A*( XH(I,J+1) - Xc(I,J) )
                               YY = YH(I,J) + A*( YH(I,J+1) - Yc(I,J) )
                            ENDIF
                         ELSE
                            A  = 0.1
                            IF (R1 .LT. R2) THEN
                               XX = XH(I,J) + A*( XH(I,J-1) - Xc(I,J) )
                               YY = YH(I,J) + A*( YH(I,J-1) - Yc(I,J) )
                            ELSE
                               XX = XH(I,J) + A*( XH(I,J+1) - Xc(I,J) )
                               YY = YH(I,J) + A*( YH(I,J+1) - Yc(I,J) )
                            ENDIF
                         ENDIF
                         Xc(I,J) = XX
                         Yc(I,J) = YY
                     ENDIF
                  ENDIF
    20      CONTINUE

         ELSE IF (ND .EQ. 0 .AND. NFLD .EQ. 9) THEN
!           horizontale linemodes

            DO 40 I = 2,MC-1
               DO 40 J = 1,NC
                  IF (I .GT. M1 .AND. I .LT. M2) THEN
                     IF (IJC(I,J) .EQ. 10 .OR. IJC(I,J) .EQ.  1 .OR. IJC(I,J) .EQ.  3 ) THEN
                         R1 = (XH(I,J) - XH(I-1,J))**2 +      &
                              (YH(I,J) - YH(I-1,J))**2
                         R2 = (XH(I,J) - XH(I+1,J))**2 +      &
                              (YH(I,J) - YH(I+1,J))**2

                         IF ( (N2-N1) .NE. 0) THEN
                            CALL SMEERFUNCTIE(I,J,I,NB(1),FR,0,1)
                         ELSE
                            FR = 1
                         ENDIF

                         IF (JS .EQ. 1) THEN
                            IF (R1 .GT. R2) THEN
                               R3 = (R1 - R2)/2
                               if ( abs(R1).lt.1d-8 ) then
                                  A = 0.5d0
                               else
                               A  = FR*CSMO*R3/R1
                               end if
                               XX = XH(I,J) + A*( XH(I-1,J) - Xc(I,J) )
                               YY = YH(I,J) + A*( YH(I-1,J) - Yc(I,J) )
                            ELSE
                               R3 = (R2 - R1)/2
                               if ( abs(R2).lt.1d-8 ) then
                                  A = 0.5d0
                               else
                                  A  = FR*CSMO*R3/R2
                               end if
                               XX = XH(I,J) + A*( XH(I+1,J) - Xc(I,J) )
                               YY = YH(I,J) + A*( YH(I+1,J) - Yc(I,J) )
                            ENDIF
                         ELSE
                            A  = 0.1
                            IF (R1 .LT. R2) THEN
                               XX = XH(I,J) + A*( XH(I-1,J) - Xc(I,J) )
                               YY = YH(I,J) + A*( YH(I-1,J) - Yc(I,J) )
                            ELSE
                               XX = XH(I,J) + A*( XH(I+1,J) - Xc(I,J) )
                               YY = YH(I,J) + A*( YH(I+1,J) - Yc(I,J) )
                            ENDIF
                         ENDIF
                         Xc(I,J) = XX
                         Yc(I,J) = YY
                     ENDIF
                  ENDIF
    40      CONTINUE

         ELSE IF (NFLD .EQ. 17) THEN
!           blockmode

            B  = CSMO
            A  = 1 - B
            DO 60 I = 1,MC
               DO 60 J = 1,NC
                  IF (I .GE. M1 .AND. I .LE. M2 .AND.          &
                      J .GE. N1 .AND. J .LE. N2 ) THEN
                     IF (IJC(I,J) .EQ. 10) THEN
                        Xc(I,J) = A*XH(I,J) + B*(XH(I-1,J) + XH(I+1,J))/4    &
                                            + B*(XH(I,J-1) + XH(I,J+1))/4
                        Yc(I,J) = A*YH(I,J) + B*(YH(I-1,J) + YH(I+1,J))/4    &
                                            + B*(YH(I,J-1) + YH(I,J+1))/4
                     ELSE IF (IJC(I,J) .GE. 1 .AND. IJC(I,J) .LE. 4)THEN
                        IF (IJC(I,J) .EQ. 1) THEN
                           XX = A*  XH(I,J) + B*                            &
                                ( XH(I-1,J) + XH(I+1,J) + XH(I,J+1) )/3
                           YY = A*  YH(I,J) + B*                            &
                                ( YH(I-1,J) + YH(I+1,J) + YH(I,J+1) )/3
                        ELSE IF (IJC(I,J) .EQ. 3) THEN
                           XX = A*  XH(I,J) + B*                            &
                                ( XH(I-1,J) + XH(I+1,J) + XH(I,J-1) )/3
                           YY = A*  YH(I,J) + B*                            &
                                ( YH(I-1,J) + YH(I+1,J) + YH(I,J-1) )/3
                        ELSE IF (IJC(I,J) .EQ. 2) THEN
                           XX = A*  XH(I,J) + B*                            &
                                ( XH(I,J-1) + XH(I,J+1) + XH(I-1,J) )/3
                           YY = A*  YH(I,J) + B*                            &
                                ( YH(I,J-1) + YH(I,J+1) + YH(I-1,J) )/3
                        ELSE IF (IJC(I,J) .EQ. 4) THEN
                           XX = A*  XH(I,J) + B*                            &
                                ( XH(I,J-1) + XH(I,J+1) + XH(I+1,J) )/3
                           YY = A*  YH(I,J) + B*                            &
                                ( YH(I,J-1) + YH(I,J+1) + YH(I+1,J) )/3
                        ENDIF
                        CALL MOVABS(XH(I,J),YH(I,J))
                        CALL LNABS(XX,YY)
                        IF (IJC(I,J) .EQ. 1 .OR. IJC(I,J) .EQ. 3) THEN
                           X21 = XH(I-1,J)
                           Y21 = YH(I-1,J)
                           X22 = XH(I+1,J)
                           Y22 = YH(I+1,J)
                        ELSEIF(IJC(I,J) .EQ. 2 .OR.IJC(I,J).EQ.4) THEN
                           X21 = XH(I,J-1)
                           Y21 = YH(I,J-1)
                           X22 = XH(I,J+1)
                           Y22 = YH(I,J+1)
                        ENDIF
                        CALL ORTPRO2(XH(I,J),YH(I,J),X21,Y21,       &
                                       XX,YY,X41,Y41,TV1,JA1)
                        CALL ORTPRO2(XH(I,J),YH(I,J),X22,Y22,       &
                                       XX,YY,X42,Y42,TV2,JA2)
                        IF (JA1 .EQ. 1 .AND. JA2 .EQ. 1) THEN
                           IF (TV2 .GT. TV1) THEN
                               Xc(I,J) = X42
                               Yc(I,J) = Y42
                            ELSE
                               Xc(I,J) = X41
                               Yc(I,J) = Y41
                            ENDIF
                        ELSE IF (JA1 .EQ. 1) THEN
                           Xc(I,J) = X41
                           Yc(I,J) = Y41
                        ELSE IF (JA2 .EQ. 1) THEN
                           Xc(I,J) = X42
                           Yc(I,J) = Y42
                        ELSE
                           Xc(I,J) = (X41 + X42)/2
                           Yc(I,J) = (Y41 + Y42)/2
                           WRITE(msgbuf,*) 'BLOCK VORM VERLIES'; call dbg_flush()
                        ENDIF
                     ENDIF
                  ENDIF
    60      CONTINUE

         ENDIF

         CALL PUTARR(Xc,XH,MMAX,NMAX)
         CALL PUTARR(Yc,YH,MMAX,NMAX)
         IF (NDRAW(8) .EQ. 0) THEN
            CALL READYY(' ', dble(K) / dble(ITSMO) )
         ELSE
            CALL TEKGRD(Xc,Yc,mmax, nmax, M1,N1,M2,N2,NCOLDG,NDRAW(38),-1,mc) ! key=-1 is unknown (but unused anyway)
         ENDIF

    10 CONTINUE

      CALL PUTARR(XH,Xc,MMAX,NMAX)
      CALL PUTARR(YH,Yc,MMAX,NMAX)
      deallocate(XH, YH)
      IF (NDRAW(8) .EQ. 0) CALL READYY(' ', -1d0 )

      RETURN
      END subroutine dosmooth
