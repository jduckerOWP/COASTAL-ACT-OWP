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

! $Id: comp_cross_time_1.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/comp_cross_time_1.f90 $

!> compute time (>0) when node x1 will cross line segment (3-4)
double precision function comp_cross_time_1(x1,x3,x4,v1,v3,v4,dclear)
   use m_missing

   implicit none

   double precision, dimension(2) :: x1, x3, x4   !< coordinates
   double precision, dimension(2) :: v1, v3, v4   !< velocities
   double precision               :: dclear       !< clearance

   double precision, dimension(2) :: xs, dn

   double precision, dimension(4) :: t, beta

   double precision, dimension(5) :: coeffs

   double precision, dimension(2) :: x13, x34, v13, v34

   double precision               :: a, b, c, det, time, DdDt
   double precision               :: e, f, g

   integer                        :: i

   double precision, external     :: cross_prod

   double precision, parameter    :: dtol = 1d-8

!  a t^2 + b t + c = 0

   x13 = x3-x1
   x34 = x4-x3
   v13 = v3-v1
   v34 = v4-v3
   a = cross_prod(v13,v34)
   b = cross_prod(x13,v34) - cross_prod(x34,v13)
   c = cross_prod(x13,x34)

   coeffs = (/0d0,0d0,a,b,c/)
!   coeffs = (/a,b,c,0d0,0d0/)

!  clearance:
!     ( a t^2 + b t + c )^2 = dclear^2 * (e t^2 + f t + g)
   if ( dclear.gt.0d0 ) then
      coeffs = (/a*a, 2d0*a*b, 2d0*a*c+b*b, 2d0*b*c, c*c /)
      e = dot_product(v34,v34)
      f = 2d0*dot_product(x34,v34)
      g = dot_product(x34,x34)

      coeffs = coeffs - dclear*dclear*(/ 0d0, 0d0, e, f, g /)
   end if

   t = DMISS
   beta = DMISS

   call comp_roots4(coeffs,t)

!   if ( t(1).ne.DMISS .and. t(2).ne.DMISS ) then
      do i=1,4
         if ( t(i).eq.DMISS ) cycle
         if ( t(i).lt.dtol ) cycle   ! positive times only
         xs = x4-x3+(v4-v3)*t(i)
         det = dot_product(xs,xs)
         if ( abs(det).gt.dtol ) then
            beta(i) = - dot_product(x3-x1+(v3-v1)*t(i),xs)/det
         end if
      end do
!   end if

   time = 1d99
   do i = 1,4
      if ( beta(i).ge.0d0 .and. beta(i).le.1d0 .and. t(i).ge.0d0 .and. t(i).ne.DMISS ) then
         if ( dclear.gt.0d0 ) then
            DdDt = ( 2d0*(a*t(i)**2 + b*t(i) + c)*(2d0*a*t(i)+b) - dclear**2*(2d0*e*t(i)+f) ) / ( 2d0*dclear*(e*t(i)**2+f*t(i)+g))
         else
            DdDt = -1d99
         end if
         if ( DdDt.lt.0d0 ) time = min(time,t(i))
      end if
   end do

   comp_cross_time_1 = time


   return
end function comp_cross_time_1
