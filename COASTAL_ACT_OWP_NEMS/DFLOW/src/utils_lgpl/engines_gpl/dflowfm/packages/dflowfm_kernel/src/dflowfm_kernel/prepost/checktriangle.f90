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

! $Id: checktriangle.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/checktriangle.f90 $

   SUBROUTINE CHECKTRIANGLE(N,JA,phimin,phimax)

   use m_samples
   use m_ec_triangle
   use network_data, only: TRIANGLEMINANGLE, TRIANGLEMAXANGLE
   use m_sferic
   use geometry_module, only: dcosphi
   use m_missing, only : dmiss, dxymis

   implicit none
   double precision :: phimin,phimax
   integer          :: n,ja

   integer :: k0, k1, k2, n0, n2, nn
   DOUBLE PRECISION :: X0, Y0, X1, Y1, X2, Y2, COSPHI, PHI
   IF ( TRIANGLEMINANGLE >= TRIANGLEMAXANGLE ) RETURN
   JA = 1
   phimin = 1d3 ; phimax = 0d0
   DO NN = 1,3
      N0 = NN - 1; IF (N0 < 1) N0 = N0 + 3
      N2 = NN + 1; IF (N2 > 3) N2 = N2 - 3
      K0 = INDX(N0,N) ; K1 = INDX(NN,N) ; K2 = INDX(N2,N)
      X0 = XS(K0) ; Y0 = YS(K0)
      X1 = XS(K1) ; Y1 = YS(K1)
      X2 = XS(K2) ; Y2 = YS(K2)
      COSPHI = DCOSPHI(X1,Y1,X0,Y0,X1,Y1,X2,Y2, jsferic, jasfer3D, dxymis)
      PHI    = ACOS(min(max(COSPHI,-1d0),1d0))*RD2DG
      phimin = min(phimin, phi)
      phimax = max(phimax, phi)
      IF (PHI < TRIANGLEMINANGLE .OR. PHI > TRIANGLEMAXANGLE ) THEN     ! TOO SHARP
         JA = 0
      ENDIF
   ENDDO
   RETURN
   END SUBROUTINE CHECKTRIANGLE
