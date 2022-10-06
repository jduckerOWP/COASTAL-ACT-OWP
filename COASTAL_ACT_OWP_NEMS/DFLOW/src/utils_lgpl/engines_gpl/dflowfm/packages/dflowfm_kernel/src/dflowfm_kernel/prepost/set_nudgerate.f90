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

! $Id: set_nudgerate.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/set_nudgerate.f90 $

!>  set nudge rates [1/s] from input in following order of preference:
!>     1. nudge time [s]
!>     2. nudge rate [NUDGE_RATE_UNIT_TO_SECI]
!>     3. uniform nudge time [s]
!>
!>  caution: will overwrite nudge_rate in 1/s
   subroutine set_nudgerate()
      use m_flowgeom, only: Ndx
      use m_flowparameters, only: Tnudgeuni
      use m_nudge
      use m_missing
      implicit none

      integer :: k

      do k=1,Ndx
         if ( nudge_time(k).eq.DMISS ) then
            if ( nudge_rate(k).ne.DMISS ) then
               nudge_rate(k) = NUDGE_RATE_UNIT_TO_SECI * nudge_rate(k)
            else if ( Tnudgeuni.gt.0d0 ) then
               nudge_rate(k) = 1d0 / Tnudgeuni
            else
               nudge_rate(k) = 0d0
            end if
         else if ( nudge_time(k).gt.0d0 ) then
            nudge_rate(k) = 1d0 / nudge_time(k)
         else
            nudge_rate(k) = 0d0
         end if
      end do

      return
   end subroutine set_nudgerate
