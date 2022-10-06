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

! $Id: arrowrcir.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/arrowrcir.f90 $

      SUBROUTINE ARROWrcir(X0,Y0,cs,sn)
      USE M_WEARELT
      implicit none
      double precision :: cs
      integer :: i
      double precision :: sn
      double precision :: x0
      double precision :: y0
      double precision :: X(3), Y(3), XR(3), YR(3)
      DATA X(1)  /0.8d0/, X(2) /1d0/, X(3) /0.8d0/,  &
           Y(1) /-0.1d0/, Y(2) /0d0/, Y(3) /0.1d0/

      DO 10 I = 1,3
         XR(I) = X0 + 3*rcir*(X(I)*CS - Y(I)*SN)
         YR(I) = Y0 + 3*rcir*(Y(I)*CS + X(I)*SN)
   10 CONTINUE

      CALL MOVABS(X0,Y0)
      CALL LNABS(XR(2),YR(2))
      CALL LNABS(XR(1),YR(1))

      CALL MOVABS(XR(2),YR(2))
      CALL LNABS(XR(3),YR(3))
      RETURN
      END
