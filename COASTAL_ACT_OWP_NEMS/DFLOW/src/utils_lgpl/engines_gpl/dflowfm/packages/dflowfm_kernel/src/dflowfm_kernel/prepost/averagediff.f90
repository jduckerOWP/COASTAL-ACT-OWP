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

! $Id: averagediff.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/averagediff.f90 $

      !> Computes the average segment size at polyline points.
      !! by averaging between left and right neighbouring points at each point.
      SUBROUTINE averageDiff(DPL, DDX, NPL)
      implicit none
      DOUBLE PRECISION, intent(in)  :: DPL(NPL) !< Accumulated distance at each point
      double precision, intent(out) :: DDX(NPL) !< Output average segment size.
      integer :: npl                            !< Nr. of polyline points.

      integer :: n

      DDX      = 0D0
      DDX(1)   = 1d0*( DPL(2)   - DPL(1)     )
      DDX(NPL) = 1d0*( DPL(NPL) - DPL(NPL-1) )

      DO N = 2, NPL-1
         DDX(N) = 0.5D0*( DPL(N+1) - DPL(N-1) )
      ENDDO

      END SUBROUTINE averageDiff
