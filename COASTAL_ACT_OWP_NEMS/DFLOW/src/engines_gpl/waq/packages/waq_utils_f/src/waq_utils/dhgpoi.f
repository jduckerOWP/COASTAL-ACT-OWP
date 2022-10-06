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

      SUBROUTINE DHGPOI( IVAR  , IARRAY, IKIND , IVINDX, IDIM1 ,
     +                   IDIM2 , IP_AR , IGRID , ISYS  , NOTOT ,
     +                   IP_VAR)
!
      IF ( IKIND .EQ. 2 ) THEN
         ISYS   = IVINDX
         NOTOT  = IDIM1
         IP_VAR = IP_AR+(IGRID-1)*IDIM1*IDIM2
      ELSEIF ( IKIND .EQ. 3 ) THEN
         ISYS   = 1
         NOTOT  = 1
         IP_VAR = IP_AR+(IGRID-1)*IDIM1*IDIM2+(IVINDX-1)*IDIM1
      ELSE
!
!        ERROR , undefined kind of array
!
         CALL GETMLU(LUREP)
         WRITE(LUREP,2000) IKIND, IARRAY, IVAR
         CALL SRSTOP(1)
      ENDIF
!
      RETURN
 2000 FORMAT ( ' ERROR: undefined kind of array :',I8,
     +        /'        array number             ',I8,
     +        /'        variable number          ',I8 )
      END
