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

! $Id: paktij.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_rgf/paktij.f90 $

     SUBROUTINE PAKTIJ(T,mmax, nmax, TH,imax,I1,I2,J1,J2,NUM)
       implicit none
!     Haal lijn uit array en geef aantal niet nul NUM
     integer :: mmax, nmax, imax, i1, i2, j1, j2, num
      double precision :: T(MMAX,NMAX),TH(IMAX)
      integer :: i, j, k, ji1
      TH = 0d0
      K   = 0
      JI1 = 0
      DO 10 I  = I1,I2
         DO 10 J  = J1,J2
         IF (T(I,J) .NE. 0) THEN
            K     = K + 1
            TH(K) = T(I,J)
         ENDIF
    10 CONTINUE
      NUM = K
      RETURN
      END subroutine paktij
