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

! $Id: getblu_from_bob.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/getblu_from_bob.f90 $

!> Determines the bed level at a u-point for a given flowlink,
!! based on the bob(1:2,L) values, the kcu link type and conveyance2D setting.
subroutine getblu_from_bob(L, iup, blu)
   use m_flowgeom, only: bob, lnx1D, kcu, jagrounlay, grounlay
   use m_flow, only:kmx
   use m_flowparameters, only: jaconveyance2D
   implicit none

   integer,          intent(in   ) :: L   !< Flow link number.
   integer,          intent(in   ) :: iup !< Upwind node index for this flow link (either 1 or 2, use 0 if irrelevant).
   double precision, intent(  out) :: blu !< Resulting bed level at u-point.

   ! NOTE: this code originates from sethu() originally.

   if (iup /= 0) then
   ! TODO: while documenting 1D2D code, we discovered the following undesirable bup:
   ! it should by default be the min(bob1/2), if conveyance2D < 1. Not yet changed.
      blu = bob(iup,L)
   else
      blu = min( bob(1,L), bob(2,L) )
   end if

   if (L <= lnx1D) then      ! 1D
      if (kcu(L) == 4 .and. jaconveyance2D >= 1) then
         blu = min( bob(1,L), bob(2,L) )
      else if (kcu(L) == 5 .or. kcu(L) == 7) then
         blu = max( bob(1,L), bob(2,L) )
      else
         blu = max( bob(1,L), bob(2,L) )
      endif
      if (jagrounlay > 0) then
         blu = blu + grounlay(L)
      endif

   else if (kmx == 0 .and. jaconveyance2D >= 1) then
      blu = min( bob(1,L), bob(2,L) )
   endif

end subroutine getblu_from_bob
   
