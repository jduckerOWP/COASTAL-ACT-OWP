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

! $Id: solve_2d.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_transport/solve_2d.f90 $

!> compose right-hand side
subroutine solve_2D(NUMCONST, Ndkx, Lnkx, vol1, kbot, ktop, Lbot, Ltop, sumhorflux, fluxver, source, sink, nsubsteps, jaupdate, ndeltasteps, sed, rhs)
   use m_flowgeom, only: Ndxi, Ndx, Lnx, Ln, ba  ! static mesh information
   use m_flowtimes, only: dts
   use timers

   implicit none

   integer,                                intent(in)    :: NUMCONST     !< number of transported quantities
   integer,                                intent(in)    :: Ndkx     !< total number of flownodes (dynamically changing)
   integer,                                intent(in)    :: Lnkx     !< total number of flowlinks (dynamically changing)
!   double precision, dimension(Ndkx),      intent(in)    :: sq       !< flux balance (inward positive)
   double precision, dimension(Ndkx),      intent(in)    :: vol1     !< volumes
   integer,          dimension(Ndkx),      intent(in)    :: kbot     !< flow-node based layer administration
   integer,          dimension(Ndkx),      intent(in)    :: ktop     !< flow-node based layer administration
   integer,          dimension(Lnkx),      intent(in)    :: Lbot     !< flow-link based layer administration
   integer,          dimension(Lnkx),      intent(in)    :: Ltop     !< flow-link based layer administration
   double precision, dimension(NUMCONST,Ndkx), intent(inout) :: sumhorflux  !< sum of horizontal fluxes
   double precision, dimension(NUMCONST,Ndkx), intent(in)    :: fluxver  !< vertical fluxes
   double precision, dimension(NUMCONST,Ndkx), intent(in)    :: source   !< sources
   double precision, dimension(NUMCONST,Ndkx), intent(in)    :: sink     !< linearized sinks
   integer,                                    intent(in)    :: nsubsteps  !< number of substeps
   integer,          dimension(Ndx),           intent(in)    :: jaupdate !< update cell (1) or not (0)
   integer,          dimension(Ndx),           intent(in)    :: ndeltasteps !< number of substeps between updates
   double precision, dimension(NUMCONST,Ndkx), intent(inout) :: sed      !< transported quantities
   double precision, dimension(NUMCONST,Ndkx)                :: rhs      ! work array: right-hand side, dim(NUMCONST,Ndkx)

   double precision, dimension(NUMCONST)                     :: thetavert

   double precision                                          :: dt_loc

   integer                                                   :: j, k

   integer(4) ithndl /0/
   if (timon) call timstrt ( "solve_2D", ithndl )

   thetavert = 0d0

   dt_loc = dts

   call make_rhs(NUMCONST, thetavert, Ndkx, Lnkx, 0, vol1, kbot, ktop, Lbot, Ltop, sumhorflux, fluxver, source, sed, nsubsteps, jaupdate, ndeltasteps, rhs)

   !$OMP PARALLEL DO         &
   !$OMP PRIVATE(k,j)        &
   !$OMP FIRSTPRIVATE(dt_loc)
   do k=1,Ndxi
      if ( nsubsteps.gt.1 ) then
         if ( jaupdate(k).eq.0 ) then
            cycle
         else
            dt_loc = dts*ndeltasteps(k)
         end if
      else
         dt_loc = dts
      end if

      do j=1,NUMCONST
         sed(j,k) = rhs(j,k) / (1d0 + dt_loc*sink(j,k))
      end do
   end do
   !$OMP END PARALLEL DO

   if (timon) call timstop( ithndl )
   return
end subroutine solve_2D
