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

! $Id: rectan.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/rectan.f90 $

subroutine rectan(hpr, br, hr, area, width, japerim, perim)
use m_flow, only : slotw1D
implicit none
integer          :: japerim
double precision :: hpr                  ! hoogte   in profiel
double precision :: br                   ! breedte van profiel
double precision :: hr                   ! hoogte  van profiel
double precision :: area                 ! wet cross sectional area
double precision :: width                ! width at water surface
double precision :: perim, hp            ! wet perimeter
if (japerim == 1) then
   hp = min(hpr, hr)
else
   hp = hpr
endif
area  = hp*br
width = br
perim = 2d0*hp + br
if (slotw1D > 0 .and. japerim == 0) then
   width = width + slotw1D
   area  = area  + slotw1D*hpr
endif
end subroutine rectan
