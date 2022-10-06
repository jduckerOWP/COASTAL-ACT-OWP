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

! $Id: bessel2wgs84.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_rgf/bessel2wgs84.f90 $

!> convert from Bessel to WGS84
subroutine bessel2wgs84(phibes, lambes, phiwgs, lamwgs)
   implicit none

   double precision, intent(in)  :: phibes, lambes
   double precision, intent(out) :: phiwgs, lamwgs

   double precision, dimension(2), parameter :: A1 = (/  9.99882860000000d-01, 3.29000000000000d-06 /)
   double precision, dimension(2), parameter :: A2 = (/ -1.25000000000000d-06, 9.99853330000000d-01 /)
   double precision, dimension(2), parameter :: b  = (/  5.12891000000000d-03, 1.83250000000000d-04 /)

   phiwgs = A1(1)*phibes + A2(1)*lambes + b(1)
   lamwgs = A1(2)*phibes + A2(2)*lambes + b(2)

end subroutine bessel2wgs84
