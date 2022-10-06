!!  Copyright (C)  Stichting Deltares, 2012-2022.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      SUBROUTINE DLWQ11 ( IOUT   , CONC   , BOUND  , ITIME  , IDT    ,
     &                    IDSTRT , IDSTOP , IDSTEP , SNAME  , MNAME  ,
     &                    CGRID  , LGRID  , NX     , NY     , NOTOT  ,
     &                             NOSYS  , IP     , ISFLAG , IDFLAG )
!
!
!     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED            : june 1988  BY M.E.Sileon
!
!     FUNCTION           : Writes concentrations in grid-layout and
!                          writes the result in IOUT .
!
!     LOGICAL UNITS      : IOUT = number of dump file
!
!     SUBROUTINES CALLED : none
!
!     PARAMETERS         :
!
!     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     IOUT    INTEGER  1           INPUT   unit number output file
!     CONC    REAL     NOTOT*?     INPUT   concentration values
!     BOUND   REAL     NOTOT*?     INPUT   boundary      values
!     ITIME   INTEGER  1           INPUT   present time in clock units
!     IDT     INTEGER  1           INPUT   time step of simulation
!     IDSTRT  INTEGER  1           INPUT   start time of dump output
!     IDSTOP  INTEGER  1           INPUT   stop time of dump output
!     IDSTEP  INTEGER  1           INPUT   time step of dump output
!     SNAME   CHAR*20  NOTOT       INPUT   names of substances
!     MNAME   CHAR*40  4           INPUT   model identhification
!     CGRID   CHAR*6   20*NY       IN/OUT  concentrations in grid layout
!     LGRID   INTEGER  NX*NY       INPUT   grid layout
!     NX      INTEGER  1           INPUT   number of columns in grid
!     NY      INTEGER  1           INPUT   number of rows in grid
!     NOTOT   INTEGER  1           INPUT   total number of systems
!     NOSYS   INTEGER  1           INPUT   number of active systems
!     IP      INTEGER  6           IN/OUT  paging structure
!     ISFLAG  INTEGER  1           INPUT   if 1 then dd-hh:mm'ss'
!     IDFLAG  INTEGER  1           OUTPUT  TRUE if dump took place
!
!
      use timers
      DIMENSION     CONC  ( NOTOT, * ) , BOUND( NOSYS, * ), IP (*)
      CHARACTER*6   CGRID ( 20, * )    , POINT , PADDER
      CHARACTER*20  SNAME ( * )
      CHARACTER*40  MNAME ( * )
      DIMENSION     LGRID ( * )
      LOGICAL       IDFLAG
      DATA          POINT / '  .   ' /
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq11", ithandl )
!
      IDFLAG = .FALSE.
      IF ( IDSTEP                     .LE. 0      ) goto 9999
      IF ( ITIME                      .LT. IDSTRT ) goto 9999
      IF ( ITIME-IDT                  .GE. IDSTOP ) goto 9999
      IF ( MOD( ITIME-IDSTRT, IDSTEP ).GE. IDT    ) goto 9999
      IDFLAG = .TRUE.
      IF ( NX*NY                      .EQ. 0      ) goto 9999
!
!         initialise the paging
!
      IF ( IP(3) .EQ. 0 ) THEN
           IP(3) = MAX(1,IP(1)/(7+(NY+5)*((NX+IP(2)-1)/IP(2))))
           IP(4) = 0
      ENDIF
!
!      repeat output for every substance
!
      DO 50 ITOT =  1 , NOTOT
!
!      Calculate maximum concentration of displayed segments
!
      CMAX = 0.0
      DO 10 I1 = 1, NY
      DO 10 I2 = 1, NX
      I3 = LGRID ( (I1-1)*NX+I2 )
      IF ( I3 .GT. 0 ) CMAX = AMAX1 ( CMAX, CONC (ITOT, I3) )
      IF ( I3 .LT. 0 .AND. ITOT .LE. NOSYS )
     *                 CMAX = AMAX1 ( CMAX, BOUND(ITOT,-I3) )
   10 CONTINUE
!
!      Calculate scale factor
!
      IF ( CMAX .LE. 0.0 ) THEN
           ISCALE = 0
      ELSE IF ( CMAX .GE. 0.5 ) THEN
              ISCALE =  AINT( ALOG10(CMAX) + 2.2E-5 )
           ELSE
              ISCALE = -AINT(-ALOG10(CMAX) - 2.2E-5 ) - 1
      ENDIF
!
!         start printing
!
      IF ( MOD(IP(4),IP(3)) .EQ. 0 ) THEN
           WRITE (IOUT,'('' '')')
           WRITE (IOUT,2040 ) ( MNAME(K),K=1,4)
      ENDIF
      IP(4) = IP(4) + 1
      CALL REPTIM ( IOUT  , ITIME , ISFLAG, -999.)
      WRITE(IOUT,2000) SNAME (ITOT) , ISCALE
!
!      Put concentration values in grid layout
!
      FACTOR = 10.0**ISCALE
      DO 40 I = 1, NX, IP(2)
         DO 30 J = 1, NY
            NEND = MIN ( NX, I+IP(2)-1 )
            DO 20 K = I, NEND
            CGRID ( K-I+1,J ) = POINT
            I3 = LGRID ( (J-1)*NX+K )
            IF ( I3 .GT. 0 ) THEN
                 WRITE ( PADDER, '(F6.3)')   CONC (ITOT, I3 )/FACTOR
                 CGRID(K-I+1,J) = PADDER
            ENDIF
            IF ( I3 .LT. 0 .AND. ITOT .LE. NOSYS ) THEN
                 WRITE ( PADDER, '(F6.3)')   BOUND(ITOT,-I3 )/FACTOR
                 CGRID(K-I+1,J) = PADDER
            ENDIF
   20       CONTINUE
            WRITE ( IOUT, 2030 ) ( CGRID ( K-I+1, J ) , K=I, NEND )
   30   CONTINUE
        WRITE ( IOUT, '('' '')' )
   40 CONTINUE
!
   50 CONTINUE
!
 9999 if ( timon ) call timstop ( ithandl )
      RETURN
!
 2000 FORMAT (/' DUMP OF RESULTS OF ',A20,
     *          ' SCALE FACTOR = 10.0 EXP (',I3,' ).'//)
 2030 FORMAT (   6X , 20A6  )
 2040 FORMAT (  45X ,   A40 )
!
      END
