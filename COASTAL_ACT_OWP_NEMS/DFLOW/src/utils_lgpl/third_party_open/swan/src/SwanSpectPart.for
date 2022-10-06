!     Contents of this file
!
!     W3ODATMD           Parameters required for partitioning model output
!     SWPARTMD           Spectral partitioning according to the watershed method
!        includes the subroutines :
!        SWPART     (interface to watershed routines)
!        PTSORT     (sort discretized image)
!        PTNGHB     (define nearest neighbours)
!        PT_FLD     (incremental flooding algorithm)
!        FIFO_ADD, FIFO_EMPTY, FIFO_FIRST  (queue management)
!        PTMEAN     (compute mean parameters)
!
      MODULE W3ODATMD
!
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2020  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Authors
!
!     41.62: Andre van der Westhuysen, from code by H.L. Tolman
!
!  1. Updates
!
!     41.62, Nov. 15: New module
!
!  2. Purpose
!
!     This module considers the parameters required for partitioned model output
!     (from the WAVEWATCH III codes w3odatmd.ftn and ww3_grid.ftn)
!
!  3. Method
!
!     MODULE construct
!
!  4. Modules used
!
!     ---
!
      IMPLICIT NONE
!
!  5. Argument variables
!
!     ---
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables
!
!     Elements of OUT6 are aliased to pointers with the same
!     name. These pointers are defined as :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      IHMAX     Int.  Public   Number of discrete spectral levels.
!      HSPMIN    Real  Public   Minimum significant height per part.
!      WSMULT    Real  Public   Multiplier for wind sea boundary.
!      WSCUT     Real  Public   Cut-off wind factor for wind seas.
!      FLCOMB    Log.  Public   Flag for combining wind seas.
!     ----------------------------------------------------------------
!
      PUBLIC

        INTEGER               :: IHMAX
        REAL                  :: HSPMIN, WSMULT, WSCUT
        LOGICAL               :: FLCOMB

        ! These hardcoded values (from ww3_grid.ftn) can be made
        ! user-defined with the OUTPUT command
        PARAMETER             (IHMAX=100, HSPMIN=0.05, WSMULT=1.7,        41.62
     &                         WSCUT=0.333, FLCOMB=.FALSE.)
!
!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
!
!     ---
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     ---
!
! 13. Source text
!
!      CONTAINS

      END MODULE W3ODATMD

      MODULE SWPARTMD
!
      USE W3ODATMD, ONLY: IHMAX, HSPMIN, WSMULT                           41.62
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2020  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Authors
!
!     41.62: Andre van der Westhuysen, based on code by Barbara Tracy
!            and H.L. Tolman, with contributions by M. Szyszka
!
!  1. Updates
!
!     41.62, Nov. 15: New module
!
!  2. Purpose
!
!     Spectral partitioning according to the watershed method
!
!  3. Method
!
!     MODULE construct
!
!  4. Modules used
!
!     ---
!
      IMPLICIT NONE
!
!  5. Argument variables
!
!     ---
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      MK, MTH   Int.  Private  Dimensions of stored neighour array
!      NEIGH     I.A.  Private  Nearest Neighbor array
!     ----------------------------------------------------------------
!      Note: IHMAX, HSPMIN, WSMULT, WSCUT and FLCOMB used from W3ODATMD
!
      PUBLIC
!
      INTEGER, PRIVATE              :: MK = -1, MTH = -1
      INTEGER, ALLOCATABLE, PRIVATE :: NEIGH(:,:)
!
!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
!
!     SWOEXA : Compute wave partitions
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     ---
!
! 13. Source text
!
      CONTAINS

!***********************************************************************
!                                                                      *
      SUBROUTINE SWPART ( SPEC, UABS, UDIR, DEPTH, WN, SPCSIG, SPCDIR,
     &                    NP, XP, DIMXP )
!                                                                      *
!***********************************************************************
!
      USE W3ODATMD, ONLY: WSCUT, FLCOMB                                   41.62
      USE SWCOMM3 , ONLY: MSC, MDC                                        41.62
      USE OCPCOMM4, ONLY: PRINTF                                          41.62
!
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2020  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Authors
!
!     41.62: Andre van der Westhuysen, based on code by Barbara Tracy
!            and H.L. Tolman, with contributions by M. Szyszka
!
!  1. Updates
!
!     41.62, Nov. 15: New subroutine
!
!  2. Purpose
!
!     Interface to watershed partitioning routines
!
!  3. Method
!
!     Watershed Algorithm of Vincent and Soille, 1991, implemented by
!     Barbara Tracy (USACE/ERDC) for NOAA/NCEP
!
!  4. Modules used
!
!     ---
!
      IMPLICIT NONE
!
!  5. Argument variables
!
!       SPEC    R.A.   I   2D spectrum E(f,theta)
!       UABS    Real   I   Wind speed
!       UDIR    Real   I   Wind direction (Cartesian)                     41.62
!       DEPTH   Real   I   Water depth
!       WN      R.A.   I   Wavenumbers for each frequency
!       SPCDIR  REAL   I   Spectral directions                            41.62
!       SPCSIG  REAL   I   Spectral frequencies                           41.62
!       NP      Int.   O   Number of partitions
!                           -1 : Spectrum without minumum energy
!                            0 : Spectrum with minumum energy
!                                but no partitions
!       XP      R.A.   O   Parameters describing partitions
!                          Entry '0' contains entire spectrum
!       DIMXP   Int.   I   Second dimension of XP

      INTEGER, INTENT(OUT)          :: NP
      INTEGER, INTENT(IN)           :: DIMXP
      REAL, INTENT(IN)              :: SPEC(MSC,MDC), WN(MSC), UABS,
     &                                 UDIR, DEPTH
      REAL, INTENT(OUT)             :: XP(7,0:DIMXP)
      REAL, INTENT(IN)              :: SPCDIR(MDC,6)                      41.62
      REAL, INTENT(IN)              :: SPCSIG(MSC)                        41.62
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables

      INTEGER                 :: IENT, ITH, IMI(MSC*MDC), IMD(MSC*MDC),
     &                           IMO(MSC*MDC), IND(MSC*MDC), NP_MAX,
     &                           IP, IT(1), INDEX(DIMXP), NWS,
     &                           IPW, IPT, ISP
      INTEGER                 :: PMAP(DIMXP)
      REAL                    :: ZP(MSC*MDC), ZMIN, ZMAX, Z(MSC*MDC),
     &                           FACT, WSMAX, HSMAX
      REAL                    :: TP(7,DIMXP)                              41.62  extended from TP(6,1:DIMXP)

!
!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
!
!     SWOEXA : Process partition output
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     To achieve minimum storage but guaranteed storage of all
!     partitions DIMXP = ((MSC+1)/2) * ((MDC-1)/2)
!
! 12. Structure
!
!     ---
!
! 13. Source text
!

!/ ------------------------------------------------------------------- /
! 0.  Initializations
!
      SAVE IENT
      DATA IENT /0/
      CALL STRACE (IENT, 'SWPART')
!
      NP     = 0
      XP     = 0.
!
! -------------------------------------------------------------------- /
! 1.  Process input spectrum
! 1.a 2-D to 1-D spectrum
!
      DO ITH=1, MDC
        ZP(1+(ITH-1)*MSC:ITH*MSC) = SPEC(:,ITH)
      END DO
!
! 1.b Invert spectrum and 'digitize'
!
      ZMIN   = MINVAL ( ZP )
      ZMAX   = MAXVAL ( ZP )
      IF ( ZMAX-ZMIN .LT. 1.E-9 ) RETURN
!
      Z      = ZMAX - ZP
!
      FACT   = REAL(IHMAX-1) / ( ZMAX - ZMIN )
      IMI    = MAX ( 1 , MIN ( IHMAX , NINT ( 1. + Z*FACT ) ) )
!
! 1.c Sort digitized image
!
      CALL PTSORT ( IMI, IND, IHMAX )
!
! -------------------------------------------------------------------- /
! 2.  Perform partitioning
! 2.a Update nearest neighbor info as needed
!
      CALL PTNGHB
!
! 2.b Incremental flooding
!
      CALL PT_FLD ( IMI, IND, IMO, ZP, NP_MAX )
!
! 2.c Compute parameters per partition
!     NP and NX initialized inside routine
!
      CALL PTMEAN ( NP_MAX, IMO, ZP, DEPTH, UABS, UDIR, WN,
     &              SPCSIG, SPCDIR, NP, XP, DIMXP, PMAP )                 41.62
!
! -------------------------------------------------------------------- /
! 3.  Sort and recombine wind seas as needed
! 3.a Sort by wind sea fraction
!
      IF ( NP .LE. 1 ) RETURN
!
      TP(:,1:NP)  = XP(:,1:NP)
      XP(:,1:NP)  = 0.
      INDEX(1:NP) = 0
      NWS         = 0
!
      DO IP=1, NP
        IT          = MAXLOC(TP(6,1:NP))
        INDEX(IP)   = IT(1)
        XP(:,IP)    = TP(:,INDEX(IP))
        IF ( TP(6,IT(1)) .GE. WSCUT ) NWS = NWS + 1
        TP(6,IT(1)) = -1.
      END DO
!
! 3.b Combine wind seas as needed and resort
!
      IF ( NWS.GT.1 .AND. FLCOMB ) THEN
          IPW    = PMAP(INDEX(1))
          DO IP=2, NWS
             IPT    = PMAP(INDEX(IP))
             DO ISP=1, MSC*MDC
                IF ( IMO(ISP) .EQ. IPT ) IMO(ISP) = IPW
             END DO
          END DO
!
          CALL PTMEAN ( NP_MAX, IMO, ZP, DEPTH, UABS, UDIR, WN,
     &                  SPCSIG, SPCDIR, NP, XP, DIMXP, PMAP )             41.62
          IF ( NP .LE. 1 ) RETURN
!
          TP(:,1:NP)  = XP(:,1:NP)
          XP(:,1:NP)  = 0.
          INDEX(1:NP) = 0
          NWS         = 0
!
          DO IP=1, NP
            IT          = MAXLOC(TP(6,1:NP))
            INDEX(IP)   = IT(1)
            XP(:,IP)    = TP(:,INDEX(IP))
            IF ( TP(6,IT(1)) .GE. WSCUT ) NWS = NWS + 1
            TP(6,IT(1)) = -1.
          END DO
!
      END IF
!
! 3.c Sort remaining fields by wave height
!
      NWS    = MIN ( 1 , NWS )
!
      TP(:,1:NP)  = XP(:,1:NP)
      XP(:,1:NP)  = 0.
!
      IF ( NWS .GT. 0 ) THEN
          XP(:,1) = TP(:,1)
          TP(1,1) = -1.
          NWS     = 1
      END IF
!
      DO IP=NWS+1, NP
        IT          = MAXLOC(TP(1,1:NP))
        XP(:,IP)    = TP(:,IT(1))
        TP(1,IT(1)) = -1.
      END DO

!>      IF (ALLOCATED(NEIGH )) DEALLOCATE(NEIGH )                           41.62
!
! -------------------------------------------------------------------- /
! 4.  End of routine
!
      RETURN
!
!     end of subroutine SWPART
      END SUBROUTINE SWPART

!***********************************************************************
!                                                                      *
      SUBROUTINE PTSORT ( IMI, IND, IHMAX )
!                                                                      *
!***********************************************************************
!
      USE SWCOMM3, ONLY: MSC, MDC                                         41.62
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2020  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Authors
!
!     41.62: Andre van der Westhuysen, based on code by Barbara Tracy
!            and H.L. Tolman
!
!  1. Updates
!
!     41.62, Nov. 15: New subroutine
!
!  2. Purpose
!
!     This subroutine sorts the image data in ascending order
!     This sort original to F.T. Tracy (2006)
!
!  3. Method
!
!     ---
!
!  4. Modules used
!
!     ---
!
      IMPLICIT NONE
!
!  5. Argument variables
!
!       IMI     I.A.   I   Input discretized spectrum
!       IND     I.A.   O   Sorted data
!       IHMAX   Int.   I   Number of integer levels
!
      INTEGER, INTENT(IN)      :: IHMAX, IMI(MSC*MDC)
      INTEGER, INTENT(OUT)     :: IND(MSC*MDC)
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables
!
      INTEGER                 :: I, IN, IV
      INTEGER                 :: NUMV(IHMAX), IADDR(IHMAX),
     &                           IORDER(MSC*MDC)
!
!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
!
!     SWPART
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     ---
!
! 13. Source text
!
! -------------------------------------------------------------------- /
! a.  Occurences per height
!
      NUMV   = 0
      DO I=1, MSC*MDC
        NUMV(IMI(I)) = NUMV(IMI(I)) + 1
      END DO
!
! -------------------------------------------------------------------- /
! b.  Starting address per height
!
      IADDR(1) = 1
      DO I=1, IHMAX-1
        IADDR(I+1) = IADDR(I) + NUMV(I)
      END DO
!
! -------------------------------------------------------------------- /
! c.  Order points
!
      DO I=1, MSC*MDC
        IV        = IMI(I)
        IN        = IADDR(IV)
        IORDER(I) = IN
        IADDR(IV) = IN + 1
      END DO
!
! -------------------------------------------------------------------- /
! d.  Sort points
!
      DO I=1, MSC*MDC
        IND(IORDER(I)) = I
      END DO
!
      RETURN
!     end of subroutine PTSORT
      END SUBROUTINE PTSORT

!***********************************************************************
!                                                                      *
      SUBROUTINE PTNGHB
!                                                                      *
!***********************************************************************
!
      USE SWCOMM3, ONLY: MSC, MDC                                         41.62
      USE OCPCOMM4, ONLY: PRINTF                                          41.62
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2020  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Authors
!
!     41.62: Andre van der Westhuysen, based on code by Barbara Tracy
!            and H.L. Tolman
!
!  1. Updates
!
!     41.62, Nov. 15: New subroutine
!
!  2. Purpose
!
!     This subroutine computes the nearest neighbors for each grid
!     point. Wrapping of directional distribution (0 to 360) is taken
!     care of using the nearest neighbor system
!
!  3. Method
!
!     ---
!
!  4. Modules used
!
!     ---
!
      IMPLICIT NONE
!
!  5. Argument variables
!
!       IMI     I.A.   I   Input discretized spectrum
!       IMD     I.A.   O   Sorted data
!       IHMAX   Int.   I   Number of integer levels
!
!     INTEGER, INTENT(IN)      :: IHMAX, IMI(MSC*MDC)
!     INTEGER, INTENT(IN)      :: IMD(MSC*MDC)
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables
!
      INTEGER                 :: IENT, N, J, I, K
!
!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
!
!     SWPART
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     ---
!
! 13. Source text
!
      SAVE IENT
      DATA IENT /0/
      CALL STRACE (IENT, 'PTNGHB')
! -------------------------------------------------------------------- /
! a.  Check on need of processing
!
      IF ( MK.EQ.MSC .AND. MTH.EQ.MDC ) RETURN
!
      IF ( MK.GT.0 ) DEALLOCATE ( NEIGH )
      ALLOCATE ( NEIGH(9,MSC*MDC) )
      MK     = MSC
      MTH    = MDC
!
! -------------------------------------------------------------------- /
! b.  Build map
!
      NEIGH  = 0
!
! ... Base loop
!
      DO N = 1, MSC*MDC
!
        J      = (N-1) / MSC + 1
        I      = N - (J-1) * MSC
        K      = 0
!
! ... Point at the left(1)
!
        IF ( I .NE. 1 ) THEN
            K           = K + 1
            NEIGH(K, N) = N - 1
        END IF
!
! ... Point at the right (2)
!
        IF ( I .NE. MSC ) THEN
            K           = K + 1
            NEIGH(K, N) = N + 1
        END IF
!
! ... Point at the bottom(3)
!
        IF ( J .NE. 1 ) THEN
            K           = K + 1
            NEIGH(K, N) = N - MSC
        END IF
!
! ... ADD Point at bottom_wrap to top
!
        IF ( J .EQ. 1 ) THEN
            K          = K + 1
            NEIGH(K,N) = MSC*MDC - (MSC-I)
        END IF
!
! ... Point at the top(4)
!
        IF ( J .NE. MDC ) THEN
            K           = K + 1
            NEIGH(K, N) = N + MSC
        END IF
!
! ... ADD Point to top_wrap to bottom
!
        IF ( J .EQ. MDC ) THEN
            K          = K + 1
            NEIGH(K,N) = N - (MDC-1) * MSC
        END IF
!
! ... Point at the bottom, left(5)
!
        IF ( (I.NE.1) .AND. (J.NE.1) ) THEN
            K           = K + 1
            NEIGH(K, N) = N - MSC - 1
        END IF
!
! ... Point at the bottom, left with wrap.
!
        IF ( (I.NE.1) .AND. (J.EQ.1) ) THEN
            K          = K + 1
            NEIGH(K,N) = N - 1 + MSC * (MDC-1)
        END IF
!
! ... Point at the bottom, right(6)
!
        IF ( (I.NE.MSC) .AND. (J.NE.1) ) THEN
            K           = K + 1
            NEIGH(K, N) = N - MSC + 1
        END IF
!
! ... Point at the bottom, right with wrap
!
        IF ( (I.NE.MSC) .AND. (J.EQ.1) ) THEN
            K           = K + 1
            NEIGH(K,N) = N + 1 + MSC * (MDC - 1)
        END  IF
!
! ... Point at the top, left(7)
!
        IF ( (I.NE.1) .AND. (J.NE.MDC) ) THEN
            K           = K + 1
            NEIGH(K, N) = N + MSC - 1
        END IF
!
! ... Point at the top, left with wrap
!
        IF ( (I.NE.1) .AND. (J.EQ.MDC) ) THEN
            K           = K + 1
            NEIGH(K,N) = N - 1 - (MSC) * (MDC-1)
        END IF
!
! ... Point at the top, right(8)
!
        IF ( (I.NE.MSC) .AND. (J.NE.MDC) ) THEN
            K           = K + 1
            NEIGH(K, N) = N + MSC + 1
        END IF
!
! ... Point at top, right with wrap
!
        IF ( (I.NE.MSC) .AND. (J.EQ.MDC) ) THEN
            K           = K + 1
            NEIGH(K,N) = N + 1 - (MSC) * (MDC-1)
        END IF
!
        NEIGH(9,N) = K
!
      END DO
!
      RETURN
!     end of subroutine PTNGHB
      END SUBROUTINE PTNGHB

!***********************************************************************
!                                                                      *
      SUBROUTINE PT_FLD ( IMI, IND, IMO, ZP, NPART )
!                                                                      *
!***********************************************************************
!
      USE SWCOMM3, ONLY: MSC, MDC                                         41.62
!
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2020  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Authors
!
!     41.62: Andre van der Westhuysen, based on code by H.L. Tolman
!
!  1. Updates
!
!     41.62, Nov. 15: New subroutine
!
!  2. Purpose
!
!     This subroutine does incremental flooding of the image to
!     determine the watershed image
!
!  3. Method
!
!     ---
!
!  4. Modules used
!
!     ---
!
      IMPLICIT NONE
!
!  5. Argument variables
!
!       IMI     I.A.   I   Input discretized spectrum
!       IND     I.A.   I   Sorted addresses
!       IMO     I.A.   O   Output partitioned spectrum
!       ZP      R.A.   I   Spectral array
!       NPART   Int.   O   Number of partitions found
!
      INTEGER, INTENT(IN)     :: IMI(MSC*MDC), IND(MSC*MDC)
      INTEGER, INTENT(OUT)    :: IMO(MSC*MDC), NPART
      REAL, INTENT(IN)        :: ZP(MSC*MDC)
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables
!
      INTEGER                 :: IENT, MASK, INIT, IWSHED, IMD(MSC*MDC),
     &                           IC_LABEL, IFICT_PIXEL, M, IH, MSAVE,
     &                           IP, I, IPP, IC_DIST, IEMPTY, IPPP,
     &                           JL, JN, IPT, J
      INTEGER                 :: IQ(MSC*MDC), IQ_START, IQ_END
      REAL                    :: ZPMAX, EP1, DIFF
!
!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
!
!     SWPART
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     ---
!
! 13. Source text
!
      SAVE IENT
      DATA IENT /0/
      CALL STRACE (IENT, 'PT_FLD')
! -------------------------------------------------------------------- /
! 0.  Initializations
!
      MASK        = -2
      INIT        = -1
      IWSHED      =  0
      IMO         = INIT
      IC_LABEL    =  0
      IMD         =  0
      IFICT_PIXEL = -100
!
      IQ_START    =  1
      IQ_END      =  1
!
      ZPMAX       = MAXVAL ( ZP )
!
! -------------------------------------------------------------------- /
! 1.  Loop over levels
!
      M      =  1
!
      DO IH=1, IHMAX
        MSAVE  = M
!
! 1.a Pixels at level IH
!
        DO
          IP     = IND(M)
          IF ( IMI(IP) .NE. IH ) EXIT
!
!     Flag the point, if it stays flagge, it is a separate minimum
!
          IMO(IP) = MASK
!
!     Consider neighbors
!     If there is neighbor, set distance and add to queue
!
          DO I=1, NEIGH(9,IP)
            IPP    = NEIGH(I,IP)
            IF ( (IMO(IPP).GT.0) .OR. (IMO(IPP).EQ.IWSHED) ) THEN
                IMD(IP) = 1
                CALL FIFO_ADD (IP)
                EXIT
            END IF
          END DO
!
          IF ( M+1 .GT. MSC*MDC ) THEN
              EXIT
          ELSE
              M = M + 1
          END IF
!
        END DO
!
! 1.b Process the queue
!
        IC_DIST = 1
        CALL FIFO_ADD (IFICT_PIXEL)
!
        DO
          CALL FIFO_FIRST (IP)
!
!     Check for end of processing
!
          IF ( IP .EQ. IFICT_PIXEL ) THEN
              CALL FIFO_EMPTY (IEMPTY)
              IF ( IEMPTY .EQ. 1 ) THEN
                  EXIT
              ELSE
                  CALL FIFO_ADD (IFICT_PIXEL)
                  IC_DIST = IC_DIST + 1
                  CALL FIFO_FIRST (IP)
              END IF
          END IF
!
!     Process queue
!
          DO I=1, NEIGH(9,IP)
            IPP = NEIGH(I,IP)
!
!     Check for labeled watersheds or basins
!
            IF ( (IMD(IPP).LT.IC_DIST) .AND. ( (IMO(IPP).GT.0) .OR.
     &           (IMO(IPP).EQ.IWSHED))) THEN
!
                IF ( IMO(IPP) .GT. 0 ) THEN
!
                    IF ((IMO(IP) .EQ. MASK) .OR. (IMO(IP) .EQ.
     &                  IWSHED)) THEN
                        IMO(IP) = IMO(IPP)
                    ELSE IF (IMO(IP) .NE. IMO(IPP)) THEN
                        IMO(IP) = IWSHED
                    END IF
!
                ELSE IF (IMO(IP) .EQ. MASK) THEN
!
                    IMO(IP) = IWSHED
!
                END IF
!
            ELSE IF ( (IMO(IPP).EQ.MASK) .AND. (IMD(IPP).EQ.0) ) THEN
!
                 IMD(IPP) = IC_DIST + 1
                 CALL FIFO_ADD (IPP)
!
            END IF
!
          END DO
!
        END DO
!
! 1.c Check for mask values in IMO to identify new basins
!
        M = MSAVE
!
        DO
          IP     = IND(M)
          IF ( IMI(IP) .NE. IH ) EXIT
          IMD(IP) = 0
!
          IF (IMO(IP) .EQ. MASK) THEN
!
! ... New label for pixel
!
              IC_LABEL = IC_LABEL + 1
              CALL FIFO_ADD (IP)
              IMO(IP) = IC_LABEL
!
! ... and all connected to it ...
!
              DO
                CALL FIFO_EMPTY (IEMPTY)
                IF ( IEMPTY .EQ. 1 ) EXIT
                CALL FIFO_FIRST (IPP)
!
                DO I=1, NEIGH(9,IPP)
                  IPPP   = NEIGH(I,IPP)
                  IF ( IMO(IPPP) .EQ. MASK ) THEN
                      CALL FIFO_ADD (IPPP)
                      IMO(IPPP) = IC_LABEL
                  END IF
                END DO
!
              END DO
!
          END IF
!
          IF ( M + 1 .GT. MSC*MDC ) THEN
              EXIT
          ELSE
              M = M + 1
          END IF
!
        END DO
!
      END DO
!
! -------------------------------------------------------------------- /
! 2.  Find nearest neighbor of 0 watershed points and replace
!     use original input to check which group to affiliate with 0
!     Soring changes first in IMD to assure symetry in adjustment
!
      DO J=1, 5
        IMD    = IMO
        DO JL=1 , MSC*MDC
          IPT    = -1
          IF ( IMO(JL) .EQ. 0 ) THEN
              EP1    = ZPMAX
              DO JN=1, NEIGH (9,JL)
                DIFF   = ABS ( ZP(JL) - ZP(NEIGH(JN,JL)))
                IF ( (DIFF.LE.EP1) .AND. (IMO(NEIGH(JN,JL)).NE.0) ) THEN
                    EP1    = DIFF
                    IPT    = JN
                END IF
              END DO
              IF ( IPT .GT. 0 ) IMD(JL) = IMO(NEIGH(IPT,JL))
          END IF
        END DO
        IMO    = IMD
        IF ( MINVAL(IMO) .GT. 0 ) EXIT
      END DO
!
      NPART = IC_LABEL
!
      RETURN
!
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE FIFO_ADD ( IV )
!
!     Add point to FIFO queue
!
      INTEGER, INTENT(IN)      :: IV
!
      IQ(IQ_END) = IV
!
      IQ_END = IQ_END + 1
      IF ( IQ_END .GT. MSC*MDC ) IQ_END = 1
!
      RETURN
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
      SUBROUTINE FIFO_EMPTY ( IEMPTY )
!
!     Check if queue is empty
!
      INTEGER, INTENT(OUT)     :: IEMPTY
!
      IF ( IQ_START .NE. IQ_END ) THEN
        IEMPTY = 0
      ELSE
        IEMPTY = 1
      END IF
!
      RETURN
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
      SUBROUTINE FIFO_FIRST ( IV )
!
!     Get point out of queue
!
      INTEGER, INTENT(OUT)     :: IV
!
      IV = IQ(IQ_START)
!
      IQ_START = IQ_START + 1
      IF ( IQ_START .GT. MSC*MDC ) IQ_START = 1
!
      RETURN
      END SUBROUTINE

!     end of subroutine PT_FLD
      END SUBROUTINE PT_FLD

!***********************************************************************
!                                                                      *
      SUBROUTINE PTMEAN ( NPI, IMO, ZP, DEPTH, UABS, UDIR, WN,
     &                    SPCSIG, SPCDIR, NPO, XP, DIMXP, PMAP )          41.62
!                                                                      *
!***********************************************************************
!
      USE SWCOMM3, ONLY: MSC, MDC, DDIR, PI2, DEGRAD, FRINTF              41.62
      USE SWCOMM1, ONLY: OUTPAR                                           41.62
      USE OCPCOMM4, ONLY: PRINTF                                          41.62
!
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2020  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Authors
!
!     41.62: Andre van der Westhuysen, based on code by Barbara Tracy
!            and H.L. Tolman, with contributions by M. Szyszka
!
!  1. Updates
!
!     41.62, Nov. 15: New subroutine
!
!  2. Purpose
!
!     Compute mean parameters per partition
!
!  3. Method
!
!     ---
!
!  4. Modules used
!
!     ---
!
      IMPLICIT NONE
!
!  5. Argument variables
!
!       NPI     Int.   I   Number of partitions found
!       IMO     I.A.   I   Partition map
!       ZP      R.A.   I   Input spectrum
!       DEPTH   Real   I   Water depth
!       UABS    Real   I   Wind speed
!       UDIR    Real   I   Wind direction (Cartesian)                     41.62
!       WN      R.A.   I   Wavenumebers for each frequency
!       SPCDIR  REAL   I   Spectral directions                            41.62
!       SPCSIG  REAL   I   Spectral frequencies                           41.62
!       NPO     Int.   O   Number of partitions with mean parameters
!       XP      R.A.   O   Array with output parameters
!       DIMXP   int.   I   Second dimesion of XP
!       PMAP    I.A.   O   Mapping between orig. and combined partitions
!
      INTEGER, INTENT(IN)     :: NPI, IMO(MSC*MDC), DIMXP
      INTEGER, INTENT(OUT)    :: NPO, PMAP(DIMXP)
      REAL, INTENT(IN)        :: ZP(MSC*MDC), DEPTH, UABS, UDIR, WN(MSC)
      REAL, INTENT(OUT)       :: XP(7,0:DIMXP)
      REAL, INTENT(IN)        :: SPCDIR(MDC,6)                            41.62
      REAL, INTENT(IN)        :: SPCSIG(MSC)                              41.62
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables
!
      INTEGER                 :: IENT, IS, ITH, ISP, IP, IFPMAX(0:NPI)
      REAL                    :: SUMF(0:MSC+1,0:NPI), SUMFW(MSC,0:NPI),
     &                           SUMFX(MSC,0:NPI), SUMFY(MSC,0:NPI),
     &                           SUME(0:NPI), SUMEW(0:NPI),
     &                           SUMEX(0:NPI), SUMEY(0:NPI),
     &                           EFPMAX(0:NPI), FCDIR(MDC)
      REAL                    :: SUMFK(MSC,0:NPI), SUMEK(0:NPI)           41.62
      REAL                    :: HS, XL, XH, XL2, XH2, EL, EH, DENOM,
     &                           SIGP, WNP, CGP, UPAR, C(MSC), RD, FACT
      REAL                    :: DS, FTE                                  41.62
!
!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
!
!     SWPART
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     ---
!
! 13. Source text
!
      SAVE IENT
      DATA IENT /0/
      CALL STRACE (IENT, 'PTMEAN')
! -------------------------------------------------------------------- /
! 1.  Check on need of processing
!
      NPO    = 0
      XP     = 0.
!
      IF ( NPI .EQ. 0 ) RETURN
!
! -------------------------------------------------------------------- /
! 2.  Initialize arrays
!
      SUMF   = 0.
      SUMFW  = 0.
      SUMFX  = 0.
      SUMFY  = 0.
      SUMFK  = 0.                                                         41.62
      SUME   = 0.
      SUMEW  = 0.
      SUMEX  = 0.
      SUMEY  = 0.
      SUMEK  = 0.                                                         41.62
      IFPMAX = 0
      EFPMAX = 0.
!
      DO IS=1, MSC
        C(IS)  = SPCSIG(IS) / WN(IS)
      END DO
!
      DO ITH=1, MDC
        UPAR   = WSMULT * UABS * MAX(0.,COS(SPCDIR(ITH,1)-DEGRAD*UDIR))
        IF ( UPAR .LT. C(MSC) ) THEN
           FCDIR(ITH) = SPCSIG(MSC)*(1+FRINTF)                            41.62
        ELSE
           DO IS=MSC-1, 2, -1
              IF ( UPAR .LT. C(IS) ) EXIT
           END DO
           RD     = (C(IS)-UPAR) / (C(IS)-C(IS+1))
           IF ( RD .LT. 0 ) THEN
              IS     = 0
              RD     = MAX ( 0., RD+1. )
           END IF
           FCDIR(ITH) = RD*SPCSIG(IS+1) + (1.-RD)*SPCSIG(IS)
        END IF
      END DO
!
! -------------------------------------------------------------------- /
! 3.  Spectral integrals and preps
! 3.a Integrals
!     NOTE: Factor DDIR only used in Hs computation
!
      DO IS=2, MSC                                                        41.62
        DO ITH=1, MDC
          ISP    = IS + (ITH-1)*MSC
          IP     = IMO(ISP)
          DS     = SPCSIG(IS)-SPCSIG(IS-1)
          FACT   = MAX ( 0. , MIN ( 1. ,
     &      1.-( FCDIR(ITH) - 0.5*(SPCSIG(IS-1)+SPCSIG(IS)) )/DS) )
          SUMF (IS, 0) = SUMF (IS, 0) + ZP(ISP)
          SUMFW(IS, 0) = SUMFW(IS, 0) + ZP(ISP) * FACT
          SUMFX(IS, 0) = SUMFX(IS, 0) + ZP(ISP) * SPCDIR(ITH,2)
          SUMFY(IS, 0) = SUMFY(IS, 0) + ZP(ISP) * SPCDIR(ITH,3)
          SUMFK(IS, 0) = SUMFK(IS, 0) + ZP(ISP) * WN(IS)**OUTPAR(3)       41.62 !WLEN - tail not added
          IF ( IP .EQ. 0 ) CYCLE
          SUMF (IS,IP) = SUMF (IS,IP) + ZP(ISP)
          SUMFW(IS,IP) = SUMFW(IS,IP) + ZP(ISP) * FACT
          SUMFX(IS,IP) = SUMFX(IS,IP) + ZP(ISP) * SPCDIR(ITH,2)
          SUMFY(IS,IP) = SUMFY(IS,IP) + ZP(ISP) * SPCDIR(ITH,3)
          SUMFK(IS,IP) = SUMFK(IS,IP) + ZP(ISP) * WN(IS)**OUTPAR(3)       41.62 !WLEN - tail not added
        END DO
      END DO
!>      SUMF(MSC+1,:) = SUMF(MSC,:) * FACHFE                                41.62 !Tail addition deativated
!
      DO IP=0, NPI
        DO IS=2, MSC                                                      41.62
          DS = SPCSIG(IS)-SPCSIG(IS-1)
!          SUME (IP) = SUME (IP) + SUMF (IS,IP) * DS
!          SUMEW(IP) = SUMEW(IP) + SUMFW(IS,IP) * DS
!          SUMEX(IP) = SUMEX(IP) + SUMFX(IS,IP) * DS
!          SUMEY(IP) = SUMEY(IP) + SUMFY(IS,IP) * DS
          !Replaced original with more accurate trapezoidal rule          41.62
          SUME (IP) = SUME (IP) + 0.5*(SPCSIG(IS)*SUMF(IS,IP) +           41.62
     &                            SPCSIG(IS-1)*SUMF(IS-1,IP))*DS          41.62
          SUMEW(IP) = SUMEW(IP) + 0.5*(SPCSIG(IS)*SUMFW(IS,IP) +          41.62
     &                            SPCSIG(IS-1)*SUMFW(IS-1,IP))*DS         41.62
          SUMEX(IP) = SUMEX(IP) + 0.5*(SPCSIG(IS)*SUMFX(IS,IP) +          41.62
     &                            SPCSIG(IS-1)*SUMFX(IS-1,IP))*DS         41.62
          SUMEY(IP) = SUMEY(IP) + 0.5*(SPCSIG(IS)*SUMFY(IS,IP) +          41.62
     &                            SPCSIG(IS-1)*SUMFY(IS-1,IP))*DS         41.62
          SUMEK(IP) = SUMEK(IP) + 0.5*(SPCSIG(IS)*SUMFK(IS,IP) +          41.62 !WLEN - tail not added
     &                            SPCSIG(IS-1)*SUMFK(IS-1,IP))*DS         41.62 !WLEN - tail not added
          IF ( SUMF(IS,IP) .GT. EFPMAX(IP) ) THEN
            IFPMAX(IP) = IS
            EFPMAX(IP) = SUMF(IS,IP)
          END IF
        END DO
      END DO
!
! -------------------------------------------------------------------- /
! 4.  Compute pars
!
      NPO    = -1
!
      DO IP=0, NPI
!
        HS     = 4. * SQRT ( SUME(IP) * DDIR )                            41.62
        IF ( HS .LT. HSPMIN ) CYCLE
!
        XL     = 1./(1+FRINTF) - 1.
        XH     = FRINTF
        XL2    = XL**2
        XH2    = XH**2
        EL     = SUMF(IFPMAX(IP)-1,IP) - SUMF(IFPMAX(IP),IP)
        EH     = SUMF(IFPMAX(IP)+1,IP) - SUMF(IFPMAX(IP),IP)
        DENOM  = XL*EH - XH*EL
        SIGP   = SPCSIG(IFPMAX(IP)) * ( 1. + 0.5 * ( XL2*EH - XH2*EL )
     &                 / SIGN ( MAX(ABS(DENOM),1.E-15) , DENOM ) )
!
        IF ( NPO .GE. DIMXP ) GOTO 2000
        NPO       = NPO + 1
        IF (IP.GT.0) PMAP(NPO) = IP
        XP(1,NPO) = HS
        XP(2,NPO) = PI2 / SIGP
!>        XP(3,NPO) = PI2 / WNP
        IF ( SUMEK(IP) .GT. 0 ) THEN                                      41.62
           XP(3,NPO) = PI2 * (SUME(IP) / SUMEK(IP)) ** (1./OUTPAR(3))     41.62
        ELSE                                                              41.62
           XP(3,NPO) = 0.                                                 41.62
        END IF                                                            41.62
        XP(4,NPO) = MOD( 630.-ATAN2(SUMEY(IP),SUMEX(IP))/DEGRAD , 360. )  41.62
        XP(5,NPO) = (1/DEGRAD) * SQRT ( MAX ( 0. , 2. * ( 1. - SQRT (
     &          MAX(0.,(SUMEX(IP)**2+SUMEY(IP)**2)/SUME(IP)**2) ) ) ) )
        XP(6,NPO) = SUMEW(IP) / SUME(IP)
        IF ( XP(3,NPO) .GT. 0. ) THEN                                     41.62
           XP(7,NPO) = HS / XP(3,NPO)                                     41.62
        ELSE                                                              41.62
           XP(7,NPO) = 0.                                                 41.62
        END IF                                                            41.62
!
      END DO
!
      RETURN
!
! Escape locations read errors --------------------------------------- *
!
 2000 CONTINUE
      WRITE (PRINTF,1000) NPO+1                                           41.62
      RETURN
!
! Formats
!
 1000 FORMAT (/' *** ERROR IN PTMEAN :'/                                  41.62
     &         '     XP ARRAY TOO SMALL AT PARTITION',I6/)

!     end of subroutine PTMEAN
      END SUBROUTINE PTMEAN
!
! End of module SWPARTMD
!
      END MODULE SWPARTMD
