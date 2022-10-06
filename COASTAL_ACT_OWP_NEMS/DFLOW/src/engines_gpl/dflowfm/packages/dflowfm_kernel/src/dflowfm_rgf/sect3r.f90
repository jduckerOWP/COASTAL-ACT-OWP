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

! $Id: sect3r.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_rgf/sect3r.f90 $

!>    compute the intersection of two splines
      SUBROUTINE SECT3R(     XI,     YI,     XJ,     YJ,    mmax, nmax, imax, CRP,   &
                          NUMPI,  NUMPJ, NUMCRO,    TIV,    TJV,  XP,     YP)

      use m_missing
      use geometry_module, only: dbdistance, cross
      use m_sferic, only: jsferic, jasfer3D

      implicit none
!     BEPAAL HET SNYPUNT VAN DE 2 SPLINES NR I EN J      USE DIMENS

      integer,                           intent(in)    :: imax       !< array size
      integer,                           intent(in)    :: mmax, nmax !< unused
      integer,                           intent(in)    :: numpi      !< number of control points of first spline
      integer,                           intent(in)    :: numpj      !< number of control points of second spline

      double precision, dimension(imax), intent(in)    :: xi, yi     !< control point coordinates of first spline
      double precision, dimension(imax), intent(in)    :: xj, yj     !< control point coordinates of second spline

      integer,                           intent(out)   :: numcro     !< number of intersections found

      double precision,                  intent(out)   :: crp        !< cross product (SPvdP: dimensional, so only look at sign)
      double precision,                  intent(out)   :: tiv        !< spline-coordinate of intersetion point on first spline (0 corresponds to first control point, 1 to second, and so on)
      double precision,                  intent(out)   :: tjv        !< spline-coordinate of intersetion point on second spline
      double precision,                  intent(out)   :: xp, yp     !< coordinates of intersection point


      double precision :: ti, tj, tip, tjp, ti0, ti1, ti2, tj0, tj1, tj2, tii, tjj, &
                          tio, tjo, &
                          timx, tjmx, eps, eps2, xcr, ycr, crs, dis, &
                          xo, yo
      double precision :: sl, sm, xi2(imax), yi2(imax), xj2(imax), yj2(imax), xc(4), yc(4)
      integer :: i, j, jo, jacros, k

      double precision :: sdist, sdistmin


      NUMCRO = 0
      EPS    = 0.0001
      EPS2   = 0.000001
      TI     = -1
      TJ     = -1

      TI0 = 0d0
      TJ0 = 0d0

      sdistmin = 1d99   ! used for selecting middle intersection in case of multiple intersections

      DO I = 1,NUMPI-1                   ! SNIJDEN RECHTE LIJNSTUKKEN?
        DO J = 1,NUMPJ-1
           XC(1) = XI(I)
           XC(2) = XI(I+1)
           XC(3) = XJ(J)
           XC(4) = XJ(J+1)
           YC(1) = YI(I)
           YC(2) = YI(I+1)
           YC(3) = YJ(J)
           YC(4) = YJ(J+1)
           CRP = -1234d0
           CALL CROSS(XC(1),YC(1),XC(2),YC(2),XC(3),YC(3),XC(4),YC(4), &
                      JACROS,SL,SM,XCR,YCR,CRP, jsferic, dmiss)
          IF (JACROS .EQ. 1) THEN
             if ( numpi.eq.2 ) then
                sdist = min(sdistmin, abs(SL-0.5d0))
             else if ( numpj.eq.2 ) then
                sdist = abs(SM-0.5d0)
             else
                sdist = sdistmin
             end if

             if ( sdist.lt.sdistmin .or. NUMCRO.eq.0 ) then
                sdistmin = sdist
                TIP    = TI
                TJP    = TJ
!                NUMCRO = NUMCRO + 1
                NUMCRO = 1
                TI0    = I - 1
                TJ0    = J - 1
                TI     = TI0 + SL
                TJ     = TJ0 + SM
             else
!                NUMCRO = NUMCRO + 1
             end if

             IF (NUMCRO .GE. 2 .AND. ABS(TIP-TI) .GT. EPS .AND. ABS(TJP-TJ) .GT. EPS ) THEN
                 RETURN
             ENDIF
          ENDIF
        ENDDO
      ENDDO

      IF ( NUMCRO .EQ. 0) THEN
        DO I = 1,NUMPI-1
          JO = -999
          DO J = 1,NUMPJ-1               ! ZO NIET, GRENZEN OPREKKEN
            IF (J .GT. JO + 1) THEN      ! NA OPREKKEN GRENZEN NIET TWEE
!              JO    = J                  ! KEER NAAST ELKAAR ZOEKEN
!             SPvdP: previous line causes asymmetries
              XC(1) = XI(I)
              XC(2) = XI(I+1)
              XC(3) = XJ(J)
              XC(4) = XJ(J+1)
              YC(1) = YI(I)
              YC(2) = YI(I+1)
              YC(3) = YJ(J)
              YC(4) = YJ(J+1)
              SL = dmiss; SM = dmiss
              CALL CROSS(XC(1),YC(1),XC(2),YC(2),XC(3),YC(3),XC(4),YC(4), &
                         JACROS,SL,SM,XCR,YCR,CRP,jsferic, dmiss)

!             BIJ NIEUW ZOEKEN MAG JE OOK NET BUITEN DE RECHTE LIJNSTUKKEN VALLEN 20-5-2003
              IF (SL .GT. -0.2 .AND. SL .LT. 1.2 .AND. SM .GT. -0.2 .AND. SM .LT. 1.2 ) THEN
                 NUMCRO = NUMCRO + 1
                 TI0    = I - 1
                 TJ0    = J - 1
                 TI     = TI0 + SL
                 TJ     = TJ0 + SM
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDIF

      TIO = TI0   ! SPvdP: TIO and TJO uninitialized, may be typing error
      TJO = TJ0

      IF (NUMCRO .EQ. 0) RETURN

      NUMCRO = 0

      TIMX  = NUMPI-1
      TJMX  = NUMPJ-1

      SL    = TI - TIO
      SM    = TJ - TJO

      TI    = MAX( 0.0, MIN(TIMX, TI))
      TJ    = MAX( 0.0, MIN(TJMX, TJ))

      CALL SPLINE(XI,NUMPI,XI2)
      CALL SPLINE(YI,NUMPI,YI2)
      CALL SPLINE(XJ,NUMPJ,XJ2)
      CALL SPLINE(YJ,NUMPJ,YJ2)

      TII = 1.0
      TJJ = 1.0
      K   = 0

    20 CONTINUE
         K   = K + 1

         IF (SL .GT. 0.  .AND. SL .LT. 1. ) THEN
            TII = 0.5*TII
         ENDIF
         IF (SM .GT. 0.  .AND. SM .LT. 1. ) THEN
            TJJ = 0.5*TJJ
         ENDIF
         TI1 = MAX( 0.0, MIN(TIMX, TI - TII/2) )
         TI2 = MAX( 0.0, MIN(TIMX, TI + TII/2) )
         TJ1 = MAX( 0.0, MIN(TJMX, TJ - TJJ/2) )
         TJ2 = MAX( 0.0, MIN(TJMX, TJ + TJJ/2) )
         TII = TI2 - TI1
         TJJ = TJ2 - TJ1

         CALL SPLINT(XI,XI2,NUMPI,TI1,XC(1))
         CALL SPLINT(YI,YI2,NUMPI,TI1,YC(1))
         CALL SPLINT(XI,XI2,NUMPI,TI2,XC(2))
         CALL SPLINT(YI,YI2,NUMPI,TI2,YC(2))
         CALL SPLINT(XJ,XJ2,NUMPJ,TJ1,XC(3))
         CALL SPLINT(YJ,YJ2,NUMPJ,TJ1,YC(3))
         CALL SPLINT(XJ,XJ2,NUMPJ,TJ2,XC(4))
         CALL SPLINT(YJ,YJ2,NUMPJ,TJ2,YC(4))

!        CALL SETCOL(14*K+26)
!        CALL MOVABS(XC(1),YC(1))
!        CALL  LNABS(XC(2),YC(2))
!        CALL MOVABS(XC(3),YC(3))
!        CALL  LNABS(XC(4),YC(4))

!        CALL RCIRC(XC(1),YC(1))
!        CALL RCIRC(XC(2),YC(2))
!        CALL RCIRC(XC(3),YC(3))
!        CALL RCIRC(XC(4),YC(4))
!        CALL TOEMAAR()

         XO = XCR
         YO = YCR
         SL = dmiss; SM = dmiss
         CRS = -1234d0
         CALL CROSS(XC(1),YC(1),XC(2),YC(2),XC(3),YC(3),XC(4),YC(4), &
                      JACROS,SL,SM,XCR,YCR,CRS, jsferic, dmiss)
         IF (SL .GT. -2.  .AND. SL .LT. 3.0 .AND. SM .GT. -2.  .AND. SM .LT. 3.0 ) THEN
            TIO = TI
            TJO = TJ

            TI  = TI1 + SL*TII
            TJ  = TJ1 + SM*TJJ

            TI  = MAX( 0.0, MIN(TIMX, TI) )
            TJ  = MAX( 0.0, MIN(TJMX, TJ) )

            IF (JACROS .EQ. 1) THEN  !ZOLANG IE NOG KRUIST WORDT UITPRODUCT BEPAALD
               NUMCRO = 1
               CRP    = CRS
            ENDIF

            IF (K .LT. 20) THEN
              IF (ABS(TI-TIO) .GT. EPS .OR. ABS(TJ-TJO) .GT. EPS) THEN
!                DIS = SQRT((XCR-XO)*(XCR-XO)+(YCR-YO)*(YCR-YO))
                dis = dbdistance(xo,yo,xcr,ycr, jsferic, jasfer3D, dmiss)
                IF (DIS .GT. EPS2) GOTO 20 ! NIET VERDER VERKLEINEN ALS PUNTEN AL BIJNA IDENTIEK
               ENDIF
            ENDIF
         ENDIF

      IF (NUMCRO .EQ. 1) THEN
         XP  = XCR
         YP  = YCR
         TIV = TI
         TJV = TJ
      ENDIF
!     CALL TOEMAAR()

      RETURN
      END subroutine sect3r
