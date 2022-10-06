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

! $Id: fboxold.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/fboxold.f90 $

      SUBROUTINE FBOXOLD(XB1,YB1,XB2,YB2)
      implicit none
      integer :: n
      integer :: ncolnow
      double precision :: xb1
      double precision :: xb2
      double precision :: yb1
      double precision :: yb2
      COMMON /COLNOW/ NCOLNOW
      REAL X(4), Y(4)
      N    = 4
      X(1) = real(XB1)
      X(2) = real(XB2)
      X(3) = real(XB2)
      X(4) = real(XB1)
      Y(1) = real(YB1)
      Y(2) = real(YB1)
      Y(3) = real(YB2)
      Y(4) = real(YB2)
      IF (NCOLNOW .GE. 0) CALL PFILLERCORE(X,Y,N)
      RETURN
      END
