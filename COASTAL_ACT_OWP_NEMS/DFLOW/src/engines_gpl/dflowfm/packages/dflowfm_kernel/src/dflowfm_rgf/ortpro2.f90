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

! $Id: ortpro2.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_rgf/ortpro2.f90 $

      SUBROUTINE ORTPRO2(X1,Y1,X2,Y2,X3,Y3,X4,Y4,TV,JA)
      implicit none
      double precision :: X1, Y1, X2, Y2, X3, Y3, X4, Y4, TV
      integer :: JA

      double precision :: DX, DY, R2

      JA = -1
      DX = X2 - X1
      DY = Y2 - Y1
      R2 = (DX*DX + DY*DY)
      TV = (X3*DX + Y3*DY - X1*DX - Y1*DY) / R2
      X4 = X1 + TV*DX
      Y4 = Y1 + TV*DY
      IF (0D0 .LE. TV .AND. TV .LE. 1D0) JA = 1
      TV = TV * SQRT(R2)
      RETURN
      END SUBROUTINE ORTPRO2
