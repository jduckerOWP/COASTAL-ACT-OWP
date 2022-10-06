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

! $Id: fm_thahbc.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/fm_thahbc.f90 $
!!--description-----------------------------------------------------------------
! FROM DELFT3D
! computes boundary values at open boundaries,
! based on thatcher harleman concept.
! - at each half timestep the bnd. condition at all boundary points are updated.
!   this is done to keep the routine as simple as possible
! - for negative times the boundary value will be
!   replaced by the inner value from r0
! - note: for z-model simulations, thahbc is completely based on the old geometry
!   corresponding to s0, so:
!   dzs1 = dzs0, s1 = s0, u1 = u0 and v1 = v0.
!
!!--pseudo code and references--------------------------------------------------
!
! thatcher, m.l. and harleman, r.f., 1972,
! "a mathematical model for the prediction of unsteady salinity intrusion
!  in estuaries",
! massachusetts institute of technology, report no 144.
!
!!--declarations----------------------------------------------------------------

   subroutine fm_thahbc()

   use m_flowexternalforcings
   use m_flowparameters
   use m_transport, only: NUMCONST, ISALT, ITEMP, ISED1, ISEDN, ITRA1, itrac2const
   use m_sediment

   implicit none

   integer :: i, iconst, ised

   if(jasal > 0 .and. nbnds>0) then
   call thconst(ISALT, nbnds, zbnds, kbnds, thtbnds, thzbnds)
   endif

   if(jatem > 0 .and. nbndtm>0) then
   call thconst(ITEMP, nbndtm, zbndtm, kbndtm, thtbndtm, thzbndtm)
   endif

   if(jased > 0 .and. nbndsd>0 .and. .not. stm_included) then
   call thconst(ISED1, nbndsd, zbndsd, kbndsd, thtbndsd, thzbndsd)
   endif

   if(allocated(bndtr)) then
      do i=1,numtracers
         iconst = itrac2const(i)
         if ( .not. allocated(bndtr(i)%z) ) then
            allocate( bndtr(i)%z(0) )
         endif
         if ( .not. allocated(bndtr(i)%k) ) then
            allocate( bndtr(i)%k(0,0) )
         endif
         if ( .not. allocated(bndtr(i)%tht) ) then
            allocate( bndtr(i)%tht(0) )
         endif
         if ( .not. allocated(bndtr(i)%thz) ) then
            allocate( bndtr(i)%thz(0) )
         endif
         call thconst(iconst, nbndtr(i), bndtr(i)%z, bndtr(i)%k, bndtr(i)%tht, bndtr(i)%thz)
      enddo
   endif
   if (jased > 0 .and. stm_included .and. allocated(bndsf)) then
      do i = 1, numfracs
         if ( .not. allocated(bndsf(i)%z) ) then
            allocate( bndsf(i)%z(0) )
         endif
         if ( .not. allocated(bndsf(i)%k) ) then
            allocate( bndsf(i)%k(0,0) )
         endif
         if ( .not. allocated(bndsf(i)%tht) ) then
            allocate( bndsf(i)%tht(0) )
         endif
         if ( .not. allocated(bndsf(i)%thz) ) then
            allocate( bndsf(i)%thz(0) )
         endif
         call thconst(i+ISED1-1, nbndsf(i), bndsf(i)%z, bndsf(i)%k, bndsf(i)%tht, bndsf(i)%thz)
      end do
   end if

   end subroutine fm_thahbc

subroutine thconst(iconst, nbnd, zbnd, kbnd, tht, thz)


   use m_transport
   use mathconsts, only: pi_hp
   use m_flow, only: kmxd, q1
   use m_flowtimes, only: dt_user
   use m_flowgeom, only: ln
   use m_flowexternalforcings
   use m_missing

   implicit none

   integer,intent(in)               :: iconst, nbnd
   integer,intent(in)               :: kbnd(5,nbnd)
   double precision, intent(inout)  :: zbnd(nbnd*kmxd), tht(nbnd), thz(nbnd*kmxd)

   double precision                 :: thfactor, rettim, q
   integer                          :: i, j, l, lf, m, n, lb, lt, ki

   if (nbnd == 0) then
      return
   endif

   thfactor = 1.0

   if(thz(1) == DMISS) then
      thz=zbnd
   endif

   do i = 1, nopenbndsect !faster, in general few TH-boundary conditions
      rettim = threttim(iconst,i)
      if(rettim <= 0d0) then
         cycle
      endif
      do j = 1, nbnd
         if(kbnd(5, j) /= i) then
            cycle
         endif
         lf = kbnd(3, j)
         q = q1(lf)
         if(q > 0d0) then !inflow condition
            tht(j) = max(tht(j) - dt_user, 0d0)
            thfactor = 0.5*(1d0 + cos((tht(j)/rettim)*pi_hp))
            call getLbotLtop(lf,lb,lt)
            do l = lb,lt
               m = (j - 1)*kmxd + (l-lb+1)
               zbnd(m) = thz(m) + thfactor*(zbnd(m)-thz(m))
            enddo
         else if(q == 0d0) then
            tht(j) = 0d0
         else           !outflow condition
            tht(j) = rettim
            call getLbotLtop(lf,lb,lt)
            do l = lb,lt
               ki = ln(2,l)                     ! internal point
               n = (j - 1)*kmxd + (l-lb+1)
               thz(n) = constituents(iconst,ki) ! translate m into a range in the constituents array :
            enddo
         endif
      enddo
   enddo
end subroutine

