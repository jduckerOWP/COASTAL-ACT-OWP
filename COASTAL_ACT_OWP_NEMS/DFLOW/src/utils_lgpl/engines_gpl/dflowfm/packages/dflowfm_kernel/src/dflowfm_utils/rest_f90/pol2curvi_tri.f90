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

! $Id: pol2curvi_tri.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/rest_f90/pol2curvi_tri.f90 $

!> construct triangle with three blocks of curvilinear grids
subroutine pol2curvi_tri(i1, i2_, i3_)
   use m_grid
   use m_gridsettings
   use m_alloc
   use m_missing
   use m_polygon
   implicit none

   integer, intent(in)   :: i1, i2_, i3_  !< first, second and third corner point in polygon, respectively

   double precision, dimension(:,:), allocatable  :: xh, yh, xg, yg

   double precision                               :: xm, ym
   double precision                               :: xia, xib, xic

   integer,                          dimension(3) :: M, N, i0, ileft, iright

   integer                                        :: i2, i3
   integer                                        :: istart, iend
   integer                                        :: numsubpol
   integer                                        :: Na, Nb, Ncc ! length of triangle sides
   integer                                        :: N1, N2, N3
   integer                                        :: isum, itri, Nh
   integer                                        :: ia, ib, ic, i, j
   integer                                        :: ipoint, idir
   integer                                        :: key

   integer                                        :: ierror

   ierror = 1

   if ( NPL.le.4 ) goto 1234

!  get start and end pointers in polygon
   call get_polstartend(NPL, XPL, YPL, i1, istart, iend)
   numsubpol = iend-istart+1

!  check if number of points is even
   if ( mod(numsubpol,2).eq.1 ) then
      call qnerror('Number of points needs to be even', ' ', ' ')
   end if

   idir = 1
   i2 = i2_
   i3 = i3_

!  get grid size and orientation
   Na  = i2-i1; if ( Na.lt.1 ) Na  = Na+numsubpol
   Nb  = i3-i2; if ( Nb.lt.1 ) Nb  = Nb+numsubpol
   Ncc = numsubpol-(Na+Nb)

   if ( Ncc.lt.1 ) then
      i2 = i3_
      i3 = i2_
      Na  = i2-i1; if ( Na.lt.1 ) Na  = Na+numsubpol
      Nb  = i3-i2; if ( Nb.lt.1 ) Nb  = Nb+numsubpol
      Ncc = numsubpol-(Na+Nb)
   end if

!  get block sizes (N1 x N3), (N2 x N3), (N1 X N2)
   isum = (Na+Nb+Ncc)/2
   N1 = isum - Ncc
   N2 = isum - Nb
   N3 = isum - Na

   if ( N1.lt.1 .or. N2.lt.1 .or. N3.lt.1 ) then
      call qnerror('unable to get block dimensions', ' ', ' ')
      goto 1234
   end if

!  compute midpoint
   ia = i1 + N1; if ( ia.gt.iend ) ia=ia-numsubpol
   ib = i2 + N3; if ( ib.gt.iend ) ib=ib-numsubpol
   ic = i3 + N2; if ( ic.gt.iend ) ic=ic-numsubpol


!  set dimensions of blocks
   M = (/ N1, N3, N2 /)
   N = (/ N3, N2, N1 /)

!  set pointers of block corners
!      ileft ------------------
!           |                  |
!           |                  |
!           |                  |
!           |------------------|
!          0                   iright

   i0     = (/ i1, i2, i3 /)
   ileft  = (/ ic, ia, ib /)
   iright = (/ ia, ib, ic /)

!   xia = dbdistance(XPL(i1),YPL(i1),XPL(ia),YPL(ia)) / dbdistance(XPL(i1),YPL(i1),XPL(i2),YPL(i2))
!   xib = dbdistance(XPL(i2),YPL(i2),XPL(ib),YPL(ib)) / dbdistance(XPL(i2),YPL(i2),XPL(i3),YPL(i3))
!   xic = dbdistance(XPL(i3),YPL(i3),XPL(ic),YPL(ic)) / dbdistance(XPL(i3),YPL(i3),XPL(i1),YPL(i1))

   xia = dble(N1)/dble(Na)
   xib = dble(N3)/dble(Nb)
   xic = dble(N2)/dble(Ncc)

   xm = ( ((1d0-xia)*XPL(i1) + xia*XPL(i2)) * xic + (1d0-xic)*XPL(i3) +    &
          ((1d0-xib)*XPL(i2) + xib*XPL(i3)) * xia + (1d0-xia)*XPL(i1) +    &
          ((1d0-xic)*XPL(i3) + xic*XPL(i1)) * xib + (1d0-xib)*XPL(i2) ) / 3d0

   ym = ( ((1d0-xia)*YPL(i1) + xia*YPL(i2)) * xic + (1d0-xic)*YPL(i3) +    &
          ((1d0-xib)*YPL(i2) + xib*YPL(i3)) * xia + (1d0-xia)*YPL(i1) +    &
          ((1d0-xic)*YPL(i3) + xic*YPL(i1)) * xib + (1d0-xib)*YPL(i2) ) / 3d0

!  allocate arrays with boundary coordinates
   Nh = max(maxval(M),maxval(N))+1
   allocate(xh(Nh,4))
   allocate(yh(Nh,4))

!  prepare grid
   MC = N1+N3+1
   NC = N2+N3+1

!  increase grid
   call increasegrid(MC,NC)
   xc = DMISS
   yc = DMISS

!  fill coordinates of blocks
   do itri=1,3

      xh = DMISS
      yh = DMISS

      ipoint = i0(itri)
      do i=1,N(itri)+1
         xh(i,1) = XPL(ipoint)
         yh(i,1) = YPL(ipoint)
         ipoint = ipoint-idir
         if ( ipoint.lt.istart ) ipoint = ipoint + numsubpol
         if ( ipoint.gt.iend )   ipoint = ipoint - numsubpol
      end do

      ipoint = i0(itri)
      do i=1,M(itri)+1
         xh(i,3) = XPL(ipoint)
         yh(i,3) = YPL(ipoint)
         ipoint = ipoint+idir
         if ( ipoint.lt.istart ) ipoint = ipoint + numsubpol
         if ( ipoint.gt.iend )   ipoint = ipoint - numsubpol
      end do

      do i=1,M(itri)+1
         xia = dble(i-1)/dble(M(itri))
         xh(i,4) = (1d0-xia) * XPL(ileft(itri)) + xia * xm
         yh(i,4) = (1d0-xia) * YPL(ileft(itri)) + xia * ym
      end do

      do i=1,N(itri)+1
         xia = dble(i-1)/dble(N(itri))
         xh(i,2) = (1d0-xia) * XPL(iright(itri)) + xia * xm
         yh(i,2) = (1d0-xia) * YPL(iright(itri)) + xia * ym
      end do

!     allocate arrays with grid coordinates
      call realloc(xg, (/M(itri)+1,N(itri)+1/), keepExisting=.false., fill=DMISS)
      call realloc(yg, (/M(itri)+1,N(itri)+1/), keepExisting=.false., fill=DMISS)

      MFAC = M(itri)
      NFAC = N(itri)

!     make block coordinates
      CALL TRANFN2( xh(1,1), xh(1,2), xh(1,3), xh(1,4),            &  ! . 3 .       . 4 .
                    yh(1,1), yh(1,2), yh(1,3), yh(1,4),            &  ! 4   2       1   2
                    Nh, M(itri)+1, N(itri)+1, xg, yg)

!     add to grid
      select case(itri)
         case(1)
            do j=1,N3+1
               do i=1,N1+1
                  xc(i,j) = xg(i,j)
                  yc(i,j) = yg(i,j)
               end do
            end do
         case(2)
            do j=1,N2+1
               do i=1,N3+1
                  xc(N1+N3+2-i,N2+N3+2-j) = xg(i,j)
                  yc(N1+N3+2-i,N2+N3+2-j) = yg(i,j)
               end do
            end do
         case(3)
            do j=1,N1+1
               do i=1,N2+1
                  xc(j,N2+N3+2-i) = xg(i,j)
                  yc(j,N2+N3+2-i) = yg(i,j)
               end do
            end do
      end select

!      key = 1
!      call tekgrid(key)
!      call qnerror(' ', ' ', ' ')

   end do

   ierror = 0
1234 continue

   if ( allocated(xh) ) deallocate(xh)
   if ( allocated(yh) ) deallocate(yh)
   if ( allocated(xg) ) deallocate(xg)
   if ( allocated(yg) ) deallocate(yg)

   return
end subroutine pol2curvi_tri
