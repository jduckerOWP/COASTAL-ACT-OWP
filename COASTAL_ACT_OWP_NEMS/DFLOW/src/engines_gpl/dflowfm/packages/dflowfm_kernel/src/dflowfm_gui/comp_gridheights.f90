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

! $Id: comp_gridheights.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/comp_gridheights.f90 $

!> compute the grid heights at grid edges on the center spline
subroutine comp_gridheights(mc, eheight, ierror)
   use m_splines
   use m_gridsettings
   use m_spline2curvi
   use m_alloc
   use m_missing

   implicit none

   integer,                                                intent(in)  :: mc       !< number of grid points
   double precision,              dimension(Nsubmax,mc-1), intent(out) :: eheight  !< edge-based grid height for each subinterval of gridlayers
   integer,                                                intent(out) :: ierror   !< 0: no error, 1: error

   double precision, allocatable, dimension(:,:) :: hL, hR
   double precision, allocatable, dimension(:) :: hL2, hR2
!   double precision,              dimension(mcs)                       :: t


   double precision, allocatable, dimension(:)                         :: sc       !  grid points in center spline coordinates
   double precision, allocatable, dimension(:,:)                       :: hgL, hgR !  grid heights at grid points
   double precision, allocatable, dimension(:)                         :: hgL_loc, hgR_loc

   double precision, allocatable, dimension(:)                         :: xlist, ylist, hlist
   integer,          allocatable, dimension(:)                         :: nlistL, nlistR, nlist_loc

   double precision                                                    :: fac, tL, tR


!  grid cross splines, per edge
   integer                                                             :: ncs, ndx
   integer,          allocatable, dimension(:)                         :: ics, idx
   double precision, allocatable, dimension(:)                         :: t
   double precision, dimension(2)                                      :: xs1, ys1
   double precision                                                    :: hmax, xe, ye, nx, ny, htot
   logical                                                             :: Lorient

   integer                                                             :: is, igL, igR, mfacmax, isL, isR, iter, MAXITER
   integer                                                             :: i, iL, iR, j, Ndum, num, NsubL, NsubR, numnew

   logical                                                             :: Lset

   double precision, external                                          :: splinelength_int

   ierror = 1

   mfacmax = mfac

   eheight(1,:)         = DMISS
   eheight(2:Nsubmax,:) = 0d0

   allocate(hL(Nsubmax,mcs), hR(Nsubmax,mcs))
   allocate(hL2(mcs), hR2(mcs))
   allocate(hgL(Nsubmax,mfacmax), hgR(Nsubmax,mfacmax))
   allocate(hgL_loc(Nsubmax), hgR_loc(Nsubmax))
   allocate(xlist(1), ylist(1), hlist(1), nlistL(1), nlistR(1), nlist_loc(1))
   allocate(ics(mcs), idx(mcs))
   allocate(t(mcs))

   do is=1,mcs
      mfac = splineprops(is)%mfac
!      if ( mfac.lt.1 ) cycle
      if ( splineprops(is)%id.ne.0 ) cycle

      igL  = splineprops(is)%iL
      igR  = splineprops(is)%iR
      ncs  = splineprops(is)%ncs

!     reallocate if necessary
      if ( ncs.gt.ubound(nlistL,1) ) then
         numnew = int(1.2d0*dble(ncs))+1
         call realloc(nlistL,numnew)
         call realloc(nlistR,numnew)
         call realloc(nlist_loc,numnew)
      end if

!     get the minimum number of subintervals in the cross splines for this center spline
      NsubL = minval(splineprops(is)%NsubL(1:ncs))
      NsubR = minval(splineprops(is)%NsubR(1:ncs))

      if ( NsubL.eq.0 ) hgL(1,1:mfac) = splineprops(is)%hmax
      if ( NsubR.eq.0 ) hgR(1,1:mfac) = splineprops(is)%hmax

!     interpolate the gridheight
!     use default settings
      hgL           = 0d0
      hgR           = 0d0
      hgL(1,1:mfac) = splineprops(is)%hmax
      hgR(1,1:mfac) = splineprops(is)%hmax

      if ( ncs.eq.1 ) then
         do i=1,NsubL
            hgL(i,1:mfac) = splineprops(is)%hR(i,1)
         end do
         do i=1,NsubR
            hgR(i,1:mfac) = splineprops(is)%hL(i,1)
         end do
      else if ( ncs.gt.1 ) then
!        use cross splines, spline interpolation

   !     allocate
         if ( .not.allocated(sc) ) then
            allocate(sc(mfac+1))
         else
            call realloc(sc, mfac+1)
         end if

!        compute center spline path length of grid points
         call nump(is,num)

!        reallocate if necessary
         if ( num.gt.ubound(xlist,1) ) then
            numnew = int(1.2d0*dble(num))+1
            call realloc(xlist, numnew)
            call realloc(ylist, numnew)
         end if
         xlist(1:num) = xsp(is,1:num)
         ylist(1:num) = ysp(is,1:num)

         sc(1) = splinelength_int(num, xlist, ylist, 0d0, sg1(igL))
         do i=1,mfac
            sc(i+1) = sc(i) + splinelength_int(num, xlist, ylist, sg1(igL+i-1), sg1(igL+i))
         end do

!        compute at edge center points
         do i=1,mfac
            sc(i) = 0.5d0*(sc(i) + sc(i+1))  ! sc(i+1) unaffected
         end do
         sc(mfac+1) = DMISS

!        compute center spline path length of cross splines
         t(1) = splinelength_int(num, xlist, ylist, 0d0, splineprops(is)%t(1))
         do i=1,ncs-1
            t(i+1) = t(i) + splinelength_int(num, xlist, ylist, splineprops(is)%t(i), splineprops(is)%t(i+1))
         end do

         nlistL(1:ncs) = splineprops(is)%NsubL(1:ncs)
         nlistR(1:ncs) = splineprops(is)%NsubR(1:ncs)

         do j=1,Nsubmax
            nlist_loc = nlistL-j
            call get_index(ncs, nlist_loc, ndx, idx)

            if ( ndx.gt.0 ) then
               hL(j,1:ncs) = splineprops(is)%hL(j,1:ncs)

!              reallocate if necessary
               if ( ndx.gt.ubound(hlist,1) ) then
                  numnew = int(1.2d0*dble(ndx))+1
                  call realloc(hlist, numnew)
               end if
               hlist(1:ndx) = hL(j,idx(1:ndx))

               call spline(hlist,ndx,hL2)

               do i=1,mfac
      !           find two nearest cross splines
      !           note that the cross splines need to be in increasing center spline coordinate order
                  iL = 1
                  tL = t(idx(iL))
                  iR = min(iL+1, ndx)   ! allowed, since ncs>1
                  tR = t(idx(iR))
                  do while ( tR.lt.sc(i) .and. iR.lt.ndx )
                     iL = iR
                     tL = tR
                     iR = iR+1
                     tR = t(idx(iR))
                     if ( iR.eq.ndx ) exit
                  end do

                  if ( abs(tR-tL).gt.1d-8 ) then
                     fac = (sc(i)-tL) / (tR-tL)
                  else
                     fac = 0d0
                     iR = iL
                  end if

                  fac = max(min(dble((iL))+fac-1d0, dble(ndx-1)),0d0)
!
                  call splint(hlist,hL2,ndx,fac,hgL(j,i))

!                 linear interpolation
!                  fac = fac+1d0-dble(iL)
!                  hgL(j,i) = (1d0-fac)*hL(j,idx(iL)) + fac*hL(j,idx(iR))
               end do   ! do i=1,mfac
            end if

            nlist_loc = nlistR-j
            call get_index(ncs, nlist_loc, ndx, idx)

            if ( ndx.gt.0 ) then
               hR(j,1:ncs) = splineprops(is)%hR(j,1:ncs)

!              reallocate if necessary
               if ( ndx.gt.ubound(hlist,1) ) then
                  numnew = int(1.2d0*dble(ndx))+1
                  call realloc(hlist, numnew)
               end if
               hlist(1:ndx) = hR(j,idx(1:ndx))

               call spline(hlist,ndx,hR2)

               do i=1,mfac
      !           find two nearest cross splines
      !           note that the cross splines need to be in increasing center spline coordinate order

                  iL = 1
                  tL = t(idx(iL))
                  iR = min(iL+1,1)   ! allowed, since ncs>1
                  tR = t(idx(iR))
                  do while ( tR.lt.sc(i) .and. iR.lt.ndx )
                     iL = iR
                     tL = tR
                     iR = iR+1
                     tR = t(idx(iR))
                     if ( iR.eq.ndx ) exit
                  end do

                  if ( abs(tR-tL).gt.1d-8 ) then
                     fac = (sc(i)-tL) / (tR-tL)
                  else
                     fac = 0d0
                     iR  = iL
                  end if

                  fac = max(min(dble((iL))+fac-1d0, dble(ndx-1)),0d0)

!                 spline interpolation between two original cross splines only
                  isL = splineprops(is)%ics(idx(iL))

                  if ( ndx.gt.1 ) then
                     isR = splineprops(is)%ics(idx(iR))
                  else
                     isR = isL
                  end if

                  call splint(hlist,hR2,ndx,fac,hgR(j,i))
               end do   ! do i=1,mfac
            end if
         end do   ! do j=1,Nsubmax

      end if

!     store grid height
      do i=1,mfac
         eheight(:,igL+i-1)    = hgL(:,i)
         eheight(:,igR+mfac-i) = hgR(:,i)
      end do

!     smooth grid heights
!      MAXITER = 0
!      do iter=1,MAXITER
!         do i=1,mfac
!            hgL(:,i) = eheight(:,igL+i-1)
!            hgR(:,i) = eheight(:,igR+mfac-i)
!         end do
!         do i=1,mfac
!            iL = max(i-1,1)
!            iR = min(i+1,mfac)
!            eheight(:,igL+i-1)    = 0.5d0*hgL(:,i) + 0.25d0*(hgL(:,iL) + hgL(:,iR))
!            eheight(:,igR+mfac-i) = 0.5d0*hgR(:,i) + 0.25d0*(hgR(:,iL) + hgR(:,iR))
!         end do
!      end do
   end do   ! do is = 1,mcs



   ierror = 0

!  error handling
1234 continue

!  restore
   mfac = mfacmax

!  deallocate
   if ( allocated(hL) )    deallocate(hL, hR)
   if ( allocated(hL2) )   deallocate(hL2, hR2)
   if ( allocated(sc) )    deallocate(sc)
   if ( allocated(hgL) )   deallocate(hgL, hgR)
   if ( allocated(hgL_loc)) deallocate(hgL_loc, hgR_loc)
   if ( allocated(xlist) ) deallocate(xlist, ylist, hlist, nlistL, nlistR, nlist_loc)
   if ( allocated(ics) )   deallocate(ics, idx)
   if ( allocated(t) )     deallocate(t)

   return

end subroutine comp_gridheights
