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

! $Id: getij.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_rgf/getij.f90 $

      SUBROUTINE GETIJ(X,XH,MMAX,NMAX,MNMAX,I1,I2,J1,J2)
      implicit none
      integer :: i
      integer :: i1
      integer :: i2
      integer :: j
      integer :: j1
      integer :: j2
      integer :: k
      integer :: mmax
      integer :: mnmax
      integer :: nmax
!     HAAL EEN LIJN (XH) UIT EEN ARRAY (X)
      DOUBLE PRECISION :: X(MMAX,NMAX),XH(MNMAX)
      K   = 0
      DO 10 J  = J1,J2
         DO 10 I  = I1,I2
            K     = K + 1
            XH(K) = X(I,J)
    10 CONTINUE
      RETURN
      END SUBROUTINE GETIJ
