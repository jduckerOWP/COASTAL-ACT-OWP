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

! --------------------------------------------------------------------------
! Routine ad hoc:
! Avoid a nasty problem with underflows on Windows 95/98.
!
! Usage:
! Call this routine once early in the program, for instance just after
! start-up.
!
! Note:
! It contains statements specific for Digital/Compaq Visual Fortran.
! This means that under UNIX you will need to comment out most of the
! code, an empty routine will suffice.
!
! Note:
! It even contains some extensions defined by Digital Visual Fortran
! --------------------------------------------------------------------------
!
      SUBROUTINE AVUNDF
!
      USE DFLIB
      INTEGER(2) STS
!
! ---------- Get the current settings of the mathematical coprocessor
!            and add zaa flag for treating underflows "benevolently"
!
      CALL GETCONTROLFPQQ( STS )
      STS = STS .OR. FPCW$UNDERFLOW
      CALL SETCONTROLFPQQ( STS )
!
! ---------- That was all. Return
!
      RETURN
      END
