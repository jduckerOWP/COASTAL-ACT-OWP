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

! $Id: update_flowanalysis_parameters.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_io/update_flowanalysis_parameters.f90 $

!> Update the cumulative flow analysis parameters and compute the flow Courant number, just before writing to the netcdf file
subroutine updateFlowAnalysisParameters()
   use m_flow
   use m_flowgeom
   use m_flowtimes
   use m_flowparameters, only: jamapFlowAnalysis

   implicit none

   integer :: n
   
   if (jamapFlowAnalysis == 0) then
      return
   end if

   do n = 1, ndx
      negativeDepths_cum(n) = negativeDepths_cum(n) + negativeDepths(n)
      noiterations_cum(n) = noiterations_cum(n) + noiterations(n)
      limitingTimestepEstimation_cum(n) = limitingTimestepEstimation_cum(n) + limitingTimestepEstimation(n)
      flowCourantNumber(n) = flowCourantNumber(n)*dts
   enddo
end subroutine updateFlowAnalysisParameters
