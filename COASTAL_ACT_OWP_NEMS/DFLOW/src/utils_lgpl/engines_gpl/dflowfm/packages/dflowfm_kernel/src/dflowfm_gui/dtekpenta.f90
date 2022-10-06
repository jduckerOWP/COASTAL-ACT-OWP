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

! $Id: dtekpenta.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/dtekpenta.f90 $

      SUBROUTINE DTEKPENTA(X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,X5,Y5,Z5,NCOL,NCOLR)
      use gridoperations
      implicit none
      integer :: ncol
      integer :: ncolr
      double precision :: zz
      double precision :: XX(5), YY(5)
      DOUBLE PRECISION X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,X5,Y5,Z5
      CALL DRIETWEE(X1,Y1,Z1,XX(1),YY(1),ZZ)
      CALL DRIETWEE(X2,Y2,Z2,XX(2),YY(2),ZZ)
      CALL DRIETWEE(X3,Y3,Z3,XX(3),YY(3),ZZ)
      CALL DRIETWEE(X4,Y4,Z4,XX(4),YY(4),ZZ)
      CALL DRIETWEE(X5,Y5,Z5,XX(5),YY(5),ZZ)
      CALL PFILLER(XX,YY,5,NCOL,NCOLR)
      RETURN
      END
