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

! $Id: dlimiter_nonequi.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_transport/dlimiter_nonequi.f90 $

!> MC limiter function for non-equidistant grid
double precision function dlimiter_nonequi(d1,d2,alpha,s)
   implicit none

   double precision, intent(in) :: d1, d2   !< left and right slopes
   double precision, intent(in) :: alpha    !< interface distance
   double precision, intent(in) :: s        !< mesh width ratio DX2/DX1

   double precision             :: r
   double precision, parameter  :: dtol=1d-16

   double precision             :: TWO1, TWO2

   dlimiter_nonequi = 0d0
   if ( d1*d2.lt.dtol ) return

   r = d1/d2    ! d1/d2

   TWO2 = 1d0/max(alpha,dtol)
   TWO1 = TWO2/max(s,dtol)

!  Monotinized Central
   dlimiter_nonequi = max(0d0, min(TWO1*r,TWO2,0.5d0*(1d0+r)) )

end function dlimiter_nonequi
