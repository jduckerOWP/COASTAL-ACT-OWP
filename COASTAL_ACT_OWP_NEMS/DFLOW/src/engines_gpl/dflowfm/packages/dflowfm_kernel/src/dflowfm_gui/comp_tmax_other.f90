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

! $Id: comp_tmax_other.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/comp_tmax_other.f90 $

!> compute maximum allowable grid layer growth time; with other grid points
subroutine comp_tmax_other(mc, jlay, xc, yc, vel, mc1, xc1, yc1, vel1, idx1, tmax, tmax1)
   use m_missing
   use m_sferic
   use m_spline2curvi, only: dtolLR
   use geometry_module, only: dbdistance

   implicit none

   integer,                             intent(in)    :: mc        !< number of grid points
   integer,                             intent(in)    :: jlay      !< grid layer index
   double precision, dimension(mc),     intent(in)    :: xc, yc    !< coordinates of grid points
   double precision, dimension(2,mc),   intent(in)    :: vel       !< velocity vector at grid points

   integer,                             intent(in)    :: mc1       !< number of other grid points
   double precision, dimension(mc1),    intent(in)    :: xc1, yc1  !< coordinates of other grid points
   double precision, dimension(2,mc1),  intent(in)    :: vel1      !< velocity vector at other grid points
   integer,          dimension(2,mc1),  intent(in)    :: idx1      !< (i,j)-indices of other grid points

   double precision, dimension(mc),     intent(inout) :: tmax      !< maximum allowable grid layer growth time
   double precision, dimension(mc1),    intent(inout) :: tmax1     !< maximum allowable other grid points growth time

!   double precision, dimension(mc-1)                  :: edge_width, edge_incr

   double precision, dimension(2)                     :: x1, x2, x3, x4, v1, v2, v3, v4   ! node coordinates and velocities
   double precision, dimension(2)                     :: xL, xR

   double precision                                   :: tmax1234

   double precision                                   :: d, d1, d2, d3, d4, dL1, dL2
   double precision                                   :: vv1, vv2, vv3, vv4, maxvv, dt

   double precision                                   :: t1, t2, t3, t4 ! cross times
   double precision                                   :: hlow2
   double precision                                   :: dclearance

   integer                                            :: i, j, i1, j1, i2, j2, ja, iL, iR, nummax, idum, imin, imax
   integer                                            :: iLL, iRR, jsferic_old

   double precision, external                         :: comp_cross_time_2

   double precision, parameter                        :: dtol = 1d-8
!   double precision, parameter                        :: dtolLR= 1d-2

!  work in model-coordinates
   jsferic_old = jsferic
   jsferic     = 0

!   define the 'neighborhood' of an edge, which is checked for collision without clearance only, measured in weshwidths
   nummax = 2*mc ! whole (partial) front gridline
!   nummax = 4

!  check for crossings with other grid
   do i=1,mc-1
      if ( xc(i).eq.DMISS .or. xc(i+1).eq.DMISS ) cycle

      x1 = (/ xc(i),   yc(i) /)
      x2 = (/ xc(i+1), yc(i+1) /)
      v1 = vel(:,i)
      v2 = vel(:,i+1)

      dL1 = dbdistance(x1(1),x1(2),x2(1),x2(2),jsferic, jasfer3D, dmiss)
!      if ( dL1.lt.dtol ) cycle

!     exclude edges that share a point
      call get_LR(mc, xc, yc, i,  iL, j)
      call get_LR(mc, xc, yc, i+1, j, iR)

      call get_LR(mc, xc, yc, iL, iLL, j)
      call get_LR(mc, xc, yc, iR, j, iRR)

      xL = (/ xc(iL), yc(iL) /)
      xR = (/ xc(iR), yc(iR) /)

!     find proximity [imin,imax] on gridline
      idum = iL
      do j=1,nummax
         call get_LR(mc, xc, yc, idum, imin, i1)
         if ( imin.eq.idum ) exit
         idum = imin
      end do

      idum = iR
      do j=1,nummax
         call get_LR(mc, xc, yc, idum, i1, imax)
         if ( imax.eq.idum ) exit
         idum = imax
      end do


      do j = 1,mc1-1
         if ( xc1(j).eq.DMISS .or. xc1(j+1).eq.DMISS ) cycle
!         if ( i.eq.j ) cycle

         x3 = (/ xc1(j),   yc1(j) /)
         x4 = (/ xc1(j+1), yc1(j+1) /)
         v3 = vel1(:,j)
         v4 = vel1(:,j+1)

         dL2 = dbdistance(x3(1),x3(2),x4(1),x4(2),jsferic, jasfer3D, dmiss)
!         if ( dL2.lt.dtolLR ) cycle

         if ( dbdistance(x1(1),x1(2),x3(1),x3(2),jsferic, jasfer3D, dmiss).lt.dtolLR .or. dbdistance(x2(1),x2(2),x4(1),x4(2),jsferic, jasfer3D, dmiss).lt.dtolLR ) cycle
         if ( dbdistance(x2(1),x2(2),x3(1),x3(2),jsferic, jasfer3D, dmiss).lt.dtolLR .or. dbdistance(x1(1),x1(2),x4(1),x4(2),jsferic, jasfer3D, dmiss).lt.dtolLR ) cycle


!         d = dbdistance(xL(1),xL(2),x3(1),x3(2)); if ( d.lt.dtolLR ) cycle
!         d = dbdistance(xL(1),xL(2),x4(1),x4(2)); if ( d.lt.dtolLR ) cycle
!         d = dbdistance(xR(1),xR(2),x3(1),x3(2)); if ( d.lt.dtolLR ) cycle
!         d = dbdistance(xR(1),xR(2),x4(1),x4(2)); if ( d.lt.dtolLR ) cycle

         d1 = dbdistance(x1(1),x1(2),x3(1),x3(2),jsferic, jasfer3D, dmiss)
         d2 = dbdistance(x2(1),x2(2),x3(1),x3(2),jsferic, jasfer3D, dmiss)
         d3 = dbdistance(x1(1),x1(2),x4(1),x4(2),jsferic, jasfer3D, dmiss)
         d4 = dbdistance(x2(1),x2(2),x4(1),x4(2),jsferic, jasfer3D, dmiss)

         if ( d1.lt.dtol .or. d2.lt.dtol .or. d3.lt.dtol .or. d4.lt.dtol ) cycle

!        compute clearance
!         dclearance = 0.5d0*max(dL1,dL2)

!        26-06-12: set clearence to 0 in all cases
         dclearance = 0d0

         i1 = idx1(1,j)
         i2 = idx1(1,j+1)
         j1 = idx1(2,j)
         j2 = idx1(2,j+1)
         if ( (i1.ge.imin.and.i1.le.imax) .or. (i2.ge.imin.and.i2.le.imax) ) then
            dclearance=0d0 ! in proximity on same gridline
         end if

!        do not include directly neighboring edges
         if ( iRR.ge.iLL ) then
            if ( ( (i1.gt.iLL .and. i1.lt.iRR) .or. (i2.gt.iLL .and. i2.lt.iRR) ) .and. j1.ge.jlay-1 .and. j2.ge.jlay-1 ) then
               continue
               cycle
            end if
         else  ! circularly connected grid
            if ( ( .not.(i1.ge.iRR .and. i1.le.iLL) .or. .not.(i2.ge.iRR .and. i2.le.iLL) ) .and. j1.ge.jlay-1 .and. j2.ge.jlay-1 ) then
               continue
               cycle
            end if
         end if

!        get a lower bound for the cross time
         hlow2 = 0.25d0*max((minval((/ d1, d2, d3, d4/)))**2 - (0.5d0*max(dL1,dL2))**2, 0d0)

!        check if the lower bounds is larger than the minimum found so far
         vv1 = sqrt(dot_product(v3-v1,v3-v1))
         vv2 = sqrt(dot_product(v3-v2,v3-v2))
         vv3 = sqrt(dot_product(v4-v1,v4-v1))
         vv4 = sqrt(dot_product(v4-v2,v4-v2))
         maxvv = maxval( (/ vv1, vv2, vv3, vv4 /) )

         if ( sqrt(hlow2)-dclearance.gt.maxvv*min(tmax(i),tmax(i+1)) ) then
            cycle ! no need to proceed
         end if

!         t1 = comp_cross_time_1(x1,x3,x4,v1,v3,v4)
!         t2 = comp_cross_time_1(x2,x3,x4,v2,v3,v4)
!         t3 = comp_cross_time_1(x3,x1,x2,v3,v1,v2)
!         t4 = comp_cross_time_1(x4,x1,x2,v4,v1,v2)

         t1 = comp_cross_time_2(x1,x3,x4,v1,v3,v4,dclearance)
         t2 = comp_cross_time_2(x2,x3,x4,v2,v3,v4,dclearance)
         t3 = comp_cross_time_2(x3,x1,x2,v3,v1,v2,dclearance)
         t4 = comp_cross_time_2(x4,x1,x2,v4,v1,v2,dclearance)

         tmax1234 = minval( (/t1, t2, t3, t4/) )

         if ( t1.eq.tmax1234 ) then
            tmax(i)    = min( tmax(i),    tmax1234 )
!            tmax1(j)   = min( tmax1(j),   tmax1234 )
!            tmax1(j+1) = min( tmax1(j+1), tmax1234 )
         else if ( t2.eq.tmax1234 ) then
            if ( tmax1234.lt.1d6 .and. i.eq.2 ) then
               continue
            end if
            tmax(i+1)  = min( tmax(i+1),  tmax1234 )
!            tmax1(j)   = min( tmax1(j),   tmax1234 )
!            tmax1(j+1) = min( tmax1(j+1), tmax1234 )
         else if ( t3.eq.tmax1234 .or. t4.eq.tmax1234 ) then
            tmax(i)   = min( tmax(i),   tmax1234 )
            tmax(i+1) = min( tmax(i+1), tmax1234 )
         end if

         if ( tmax1234.eq.0d0 ) exit

      end do
   end do

   jsferic = jsferic_old

   return
end subroutine comp_tmax_other
