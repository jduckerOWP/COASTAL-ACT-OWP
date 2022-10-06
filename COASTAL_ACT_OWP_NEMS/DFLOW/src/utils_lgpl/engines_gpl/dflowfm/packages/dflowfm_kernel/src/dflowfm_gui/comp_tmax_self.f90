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

! $Id: comp_tmax_self.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/comp_tmax_self.f90 $

!> compute maximum allowable grid layer growth time; self crossings
subroutine comp_tmax_self(mc, xc, yc, vel, tmax)

   use m_missing
   use geometry_module, only: dbdistance
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   integer,                           intent(in)    :: mc       !< number of grid points
   double precision, dimension(mc),   intent(in)    :: xc, yc   !< coordinates of grid points
   double precision, dimension(2,mc), intent(in)    :: vel      !< velocity vector at grid points

   double precision, dimension(mc-1), intent(inout) :: tmax   !< maximum allowable grid layer growth time

   double precision, dimension(mc-1)                :: edge_width, edge_incr

   double precision                                 :: dt

   integer                                          :: i, jsferic_old

   double precision, parameter                      :: dtol=1d-8

   double precision, external                       ::  dprodin

!  work in model-coordinates
   jsferic_old = jsferic
   jsferic     = 0

!  take unit time-step for edge length increase
   dt = 1d0

!  check for self-crossing
   edge_incr = 1d99
   do i=1,mc-1
      if ( xc(i).eq.DMISS .or. xc(i+1).eq.DMISS ) cycle

      edge_width(i) = dbdistance(xc(i),yc(i),xc(i+1),yc(i+1), jsferic, jasfer3D, dmiss)
      if ( edge_width(i).lt.dtol) cycle
      edge_incr(i) = dprodin(xc(i),yc(i),xc(i+1),yc(i+1),xc(i)+dt*vel(1,i),yc(i)+dt*vel(2,i),xc(i+1)+dt*vel(1,i+1),yc(i+1)+dt*vel(2,i+1))/edge_width(i) - edge_width(i)
      edge_incr(i) = edge_incr(i)/dt
   end do

   do i=1,mc-1
      if ( edge_incr(i).lt.0d0 ) then
         tmax(i) = -edge_width(i)/edge_incr(i)
      end if
   end do

   jsferic = jsferic_old

   return
end subroutine comp_tmax_self
