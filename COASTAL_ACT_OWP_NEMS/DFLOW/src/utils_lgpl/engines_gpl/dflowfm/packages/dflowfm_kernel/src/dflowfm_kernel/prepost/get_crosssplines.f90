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

! $Id: get_crosssplines.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/get_crosssplines.f90 $

!> get the intersections of a spline with all other splines
subroutine get_crosssplines(num, xs1, ys1, ncs, ics, Lorient, t, cosphi)
   use m_splines
   use m_spline2curvi
   use m_alloc
   use sorting_algorithms, only: indexx
   use geometry_module, only: dbdistance

   implicit none

   integer,                          intent(in)  :: num           !< number of spline control points
   double precision, dimension(num), intent(in)  :: xs1, ys1      !< coordinates of spline control points

   integer,                          intent(out) :: ncs           !< number of cross splines
   integer,          dimension(mcs), intent(out) :: ics           !< indices of the cross splines
   logical,          dimension(mcs), intent(out) :: Lorient       !< orientation
   double precision, dimension(mcs), intent(out) :: t             !< center-spline coordinate of cross splines
   double precision, dimension(mcs), intent(out) :: cosphi        !< cos of crossing angles

   double precision, dimension(:), allocatable   :: xlist, ylist


   integer,          dimension(mcs)              :: perm          ! for sorting the cross splines
   integer,          dimension(mcs)              :: ics1
   logical,          dimension(mcs)              :: Lorient1
   double precision, dimension(mcs)              :: t1

   integer                                       :: idum, idx, js, numj, numcro, numnew

   double precision                              :: crp, tj, xp, yp, hsumL, hsumR, hmax, tt

!  allocate
   allocate(xlist(1), ylist(1))

!  find the cross splines
   ncs = 0
   ics = 0
   t   = 1d99 ! default values will cause sorting to disregard non cross splines
   do js = 1,mcs
      call nump(js,numj)

!     reallocate if necessary
      if (numj.gt.ubound(xlist,1) ) then
         numnew = int(1.2d0*dble(numj)) + 1
         call realloc(xlist, numnew)
         call realloc(ylist, numnew)
      end if

!     non-cross splines may only cross with cross splines visa versa
      if ( (num.eq.2 .and. numj.eq.2) .or. (num.gt.2 .and. numj.gt.2) ) cycle

!     get the intersection of the splines
      xlist(1:numj) = xsp(js,1:numj)
      ylist(1:numj) = ysp(js,1:numj)
      call sect3r(xs1, ys1, xlist, ylist,  &
                  1, 1, max(num,numj), crp, num, numj, numcro, tt, tj, xp, yp)

      if ( abs(crp).lt.dtolcos ) then
         numcro = 0d0
      end if

      if ( numcro.eq.1 ) then   ! intersection found
         ncs = ncs+1
         ics(js) = js
         if ( crp.gt.0d0 ) then
            Lorient(js) = .false.
         else
            Lorient(js) = .true.
         end if
         t(js)      = tt
         cosphi(js) = crp
      end if

   end do   ! do js=1,mcs

!  sort cross splines, such that they are in increasing center spline coordinate order
   call indexx(mcs,t,perm)

   ics1      = ics
   Lorient1 = Lorient
   t1       = t

   do js=1,mcs
      idx = perm(js)
      ics(js)     = ics1(idx)
      Lorient(js) = Lorient1(idx)
      t(js)       = t1(idx)
   end do

!  deallocate
   deallocate(xlist, ylist)

   return
end subroutine get_crosssplines
