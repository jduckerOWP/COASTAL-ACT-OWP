!!  Copyright (C)  Stichting Deltares, 2021-2022.
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

      SUBROUTINE DHAGGR ( NOSEG1, NOSEG2, NOTOTI, NOTOTW, NOTOTH,
     +                    NOTOTO, ISYSI , ISYSW , ISYSH , ISYSO ,
     +                    IPGRID, IAGTYP, ARRINP, WEIGHT, ARRHLP,
     +                    ARROUT)
C
C     DELFT HYDRAULICS
C
C     Created             : June 1998 by Jan van Beek
C
C     Function            : Aggregates value to coarser grid
C
C     Subroutines called  : GETMLU, Get unit number report file
C                           SRSTOP, Stops execution
C                           ZERO  , Zero's a real array
C
C     Arguments           :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOSEG1  INTEGER  1          INPUT   Number of segments on finer grid
C     NOSEG2  INTEGER  1          INPUT   Number of segments on coarser grid
C     IPGRID  INTEGER  NOSEG1     INPUT   Grid pointers to coarser grid
C     IAGTYP  INTEGER  1          INPUT   Aggregation type
C                                         1 = Accumulation
C                                         2 = Average
C                                         3 = Average weighted with WEIGHT
C     ARRINP  REAL     NOSEG1     INPUT   Array to be aggregated
C     WEIGHT  REAL     NOSEG1     INPUT   Weigth in averaging
C     ARRHLP  REAL     NOSEG2     LOCAL   Local help array
C     ARROUT  REAL     NOSEG2     OUTPUT  Aggregated array
C
C     Declaration of arguments
C
      INTEGER        NOSEG1, NOSEG2, NOTOTI, NOTOTW, NOTOTH,
     +               NOTOTO, ISYSI , ISYSW , ISYSH , ISYSO ,
     +               IAGTYP
      INTEGER        IPGRID(NOSEG1)
      REAL           ARRINP(NOTOTI,NOSEG1) , WEIGHT(NOTOTW,NOSEG1) ,
     +               ARRHLP(NOTOTH,NOSEG2) , ARROUT(NOTOTO,NOSEG2)
C
C     Local declaration
C
C     ISEG1   INTEGER  1          LOCAL   Segment index finer grid
C     ISEG2   INTEGER  1          LOCAL   Segment index coarser grid
C     LUREP   INTEGER  1          LOCAL   Unit number report file
C
      INTEGER        ISEG1 , ISEG2 , LUREP
      real                     :: vmin          ! minimum in weight variable
      logical                  :: lfound        ! command line option found
      integer                  :: idummy        !
      character                :: cdummy        !
      integer                  :: ierr2         !
      integer                  :: lunrep        ! report file
      logical                  :: lfirst = .true.
      real, parameter          :: rmiss = -999.

      save           lfirst, vmin


      if ( lfirst ) then
         lfirst = .false.
         call getcom('-vmin',2,lfound,idummy,vmin,cdummy,ierr2)
         if ( lfound ) then
            call getmlu(lunrep)
            if ( ierr2 .ne. 0 ) then
               write(*,*) 'error commandline option -vmin value could not be interpreted'
               write(lunrep,*) 'error commandline option -vmin value could not be interpreted'
               call srstop(1)
            endif
            write(*,*) ' commandline option -vmin ',vmin
            write(lunrep,*) ' commandline option -vmin ',vmin
         else
            vmin = 0.0
         endif
      endif
C
C     Zero accumulation arrays
C
      IF ( IAGTYP .EQ. 1 ) THEN
         DO ISEG2 = 1 , NOSEG2
            ARROUT(ISYSO,ISEG2) = 0.0
         ENDDO
      ELSEIF ( IAGTYP .EQ. 2 .OR. IAGTYP .EQ. 3 ) THEN
         DO ISEG2 = 1 , NOSEG2
            ARROUT(ISYSO,ISEG2) = 0.0
            ARRHLP(ISYSH,ISEG2) = 0.0
         ENDDO
      ELSEIF ( IAGTYP .EQ. 4) THEN
         ! minimum assume initialised
      ENDIF
C
C     Accumulate
C
      IF ( IAGTYP .EQ. 1 ) THEN
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               ARROUT(ISYSO,ISEG2) = ARROUT(ISYSO,ISEG2) +
     +                               ARRINP(ISYSI,ISEG1)
            ENDIF
         ENDDO
      ELSEIF ( IAGTYP .EQ. 2 ) THEN
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               ARROUT(ISYSO,ISEG2) = ARROUT(ISYSO,ISEG2) +
     +                               ARRINP(ISYSI,ISEG1)
               ARRHLP(ISYSH,ISEG2) = ARRHLP(ISYSH,ISEG2) + 1.0
            ENDIF
         ENDDO
      ELSEIF ( IAGTYP .EQ. 3 ) THEN
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               IF ( WEIGHT(ISYSW,ISEG1) .GT. VMIN ) THEN
                  IF ( ARRINP(ISYSI,ISEG1) .NE. RMISS ) THEN
                     ARROUT(ISYSO,ISEG2) = ARROUT(ISYSO,ISEG2) +
     +                                     ARRINP(ISYSI,ISEG1) *
     +                                     WEIGHT(ISYSW,ISEG1)
                     ARRHLP(ISYSH,ISEG2) = ARRHLP(ISYSH,ISEG2) +
     +                                     WEIGHT(ISYSW,ISEG1)
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ELSEIF ( IAGTYP .EQ. 4 ) THEN
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               IF ( WEIGHT(ISYSW,ISEG1) .GT. VMIN ) THEN
                  IF ( ARRINP(ISYSI,ISEG1) .NE. RMISS ) THEN
                     IF ( ARROUT(ISYSO,ISEG2) .NE. RMISS ) THEN
                        ARROUT(ISYSO,ISEG2) = MIN(ARROUT(ISYSO,ISEG2),ARRINP(ISYSI,ISEG1))
                     ELSE
                        ARROUT(ISYSO,ISEG2) = ARRINP(ISYSI,ISEG1)
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ELSE
         CALL GETMLU(LUREP)
         WRITE(LUREP,2000) IAGTYP
         CALL SRSTOP(1)
      ENDIF
C
C     Average
C
      IF ( IAGTYP .EQ. 2 .OR. IAGTYP .EQ. 3 ) THEN
         DO ISEG2 = 1 , NOSEG2
            IF ( ABS(ARRHLP(ISYSH,ISEG2)) .GT. 1.E-20 ) THEN
               ARROUT(ISYSO,ISEG2) = ARROUT(ISYSO,ISEG2) /
     +                               ARRHLP(ISYSH,ISEG2)
            ELSE
               ARROUT(ISYSO,ISEG2) = 0.0
            ENDIF
         ENDDO
      ENDIF
C
      RETURN
 2000 FORMAT ( ' ERROR: undefind aggregation type in DHAGGR :',I8 )
      END
