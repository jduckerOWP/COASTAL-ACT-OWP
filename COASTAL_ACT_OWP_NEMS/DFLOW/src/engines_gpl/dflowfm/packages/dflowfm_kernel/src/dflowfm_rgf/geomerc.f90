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

! $Id: geomerc.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_rgf/geomerc.f90 $

      SUBROUTINE GEOMERC(XG,YG,XX,YY)
      USE M_SFERIC
      implicit none
      double precision :: XX,YY,XG,YG,FI2,YC,CY,F,E
      double precision :: a
      double precision :: sf
      XX  = XG*DG2RD*RA

      FI2 = DG2RD*YG
      YY  = ( 1D0 + SIN(FI2) ) / COS(FI2)
      YY  = RA*LOG(YY)

      A    = 6378140
      XX   = XG*DG2RD*RA
      YC   = DG2RD*(90-YG)
      CY   = COS(YC)
      F    = 298.257223d0
      E    = SQRT(2/F - 1/F**2)

      YY   = -A*log( ABS(tan(YC/2)) *((1+e*CY) / (1-e*CY))**(e/2) )
      SF   = sin(YC) / SQRT( 1 - E*E*CY )

      RETURN
      END SUBROUTINE GEOMERC
