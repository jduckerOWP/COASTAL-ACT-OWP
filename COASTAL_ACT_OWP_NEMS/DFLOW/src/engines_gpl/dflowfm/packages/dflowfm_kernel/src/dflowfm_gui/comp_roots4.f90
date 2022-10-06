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

! $Id: comp_roots4.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/comp_roots4.f90 $

!> solves the quartic equation Ax^4+Bx^3+Cx^2+Dx+E=0
subroutine comp_roots4(coeffs,x)
   use m_missing
   use Solve_Real_Poly

   implicit none

   double precision, dimension(5), intent(in)  :: coeffs          !< coefficient vector (A,B,C,D,E)
   double precision, dimension(4), intent(out) :: x               !< roots

   double precision, dimension(4)              :: re, im          !< real and imaginairy parts of zeros

   double precision                            :: rhs

   logical                                     :: Lfail

   double precision                            :: dtol = 1d-12

   integer                                     :: i, j, ndegree

   x = DMISS

   Lfail = .true.
!
!   ndegree = 4
!   do i=1,4
!      if ( abs(coeffs(i)).gt.dtol ) exit
!      ndegree = ndegree-1
!   end do
!
!   if ( ndegree.ge.1 ) then
!      call rpoly(coeffs(5-ndegree:5), ndegree, re(1:ndegree), im(1:ndegree), Lfail)
!   end if

   do i=4,1,-1
      ndegree = i
      if ( abs(coeffs(5-ndegree)).lt.dtol ) cycle
      call rpoly(coeffs(5-ndegree:5), ndegree, re(1:ndegree), im(1:ndegree), Lfail)
      exit
!      if ( .not.Lfail ) exit
   end do

   if ( Lfail .and. ndegree.gt.0 ) then
      return
   end if

!   do i=1,ndegree
!      if ( abs(im(i)).lt.dtol ) then
!         x(i) = re(i)
!      end if
!   end do

!   check validity of roots
!    do i=1,ndegree
!      rhs=0d0
!      do j=ndegree,0,-1
!         rhs = rhs + coeffs(5-j)*re(i)**j
!      end do
!      if ( abs(rhs).lt.dtol ) then
!         x(i) = re(i)
!      end if
!    end do



   do i=1,ndegree
      if ( abs(im(i)).lt.1d-4 ) then
         x(i) = re(i)
      end if
   end do

end subroutine comp_roots4
