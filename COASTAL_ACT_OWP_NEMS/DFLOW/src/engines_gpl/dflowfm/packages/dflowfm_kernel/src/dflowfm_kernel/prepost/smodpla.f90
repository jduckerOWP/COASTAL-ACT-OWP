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

! $Id: smodpla.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/smodpla.f90 $

      SUBROUTINE SMODPLA(DPLA, DXS, NPL)                   ! SMOOTH WITH DESIRED
      USE M_ALLOC
      implicit none
      DOUBLE PRECISION              :: DPLA(NPL), DXS(NPL)
      DOUBLE PRECISION, ALLOCATABLE :: DH(:)
      integer :: npl

      double precision :: a1
      double precision :: a2
      integer :: k
      integer :: n


      CALL REALLOC(DH,NPL)

      DO K = 1,5

         DH = DPLA
         DO N  = 2,NPL-1
            a1 = 0.5d0*( dxs(n-1) + dxs(N) )
            a2 = 0.5d0*( dxs(n+1) + dxs(N) )
            DPLA(N) =  ( a2*DH(N-1) + a1*DH(N+1) ) / ( a2 + a1 )
         ENDDO

      ENDDO

      DEALLOCATE(DH)

      END SUBROUTINE SMODPLA
