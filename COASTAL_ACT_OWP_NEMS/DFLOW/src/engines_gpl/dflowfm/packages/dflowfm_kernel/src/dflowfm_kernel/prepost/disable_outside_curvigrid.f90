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

! $Id: disable_outside_curvigrid.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/disable_outside_curvigrid.f90 $

!> disable network nodes/samples outside curvilinear grid
subroutine disable_outside_curvigrid(Nk, Ns, xk, yk, xs, ys, imaskk, imasks)
   use m_grid
   use m_polygon
   use m_missing
   use geometry_module, only: dbpinpol

   implicit none

   integer,                         intent(in)    :: Nk      !< number of network nodes
   integer,                         intent(in)    :: Ns      !< number of samples

   double precision, dimension(Nk), intent(in)    :: xk, yk  !< network node coordinates
   double precision, dimension(Ns), intent(in)    :: xs, ys  !< sample  coordinates
   integer,          dimension(Nk), intent(out)   :: imaskk  !< network nodes inside curvigrid (1) or not (0)
   integer,          dimension(Ns), intent(out)   :: imasks  !< samples       inside curvigrid (1) or not (0)

   integer                                        :: i
   integer                                        :: in

   integer                                        :: ierror

   ierror = 1

   imaskk = 0
   imasks = 0

!  store polygon
   call savepol()

!  delete polygon
   call delpol()

!  copy curvigrid boundaries to polygon
   call copycurvigridboundstopol()

   in = -1

   do i=1,Nk
      call dbpinpol(xk(i), yk(i), in,dmiss, JINS, NPL, xpl, ypl, zpl)
      if ( in.eq.1) then
         imaskk(i) = 1
      end if
   end do

   do i=1,Ns
      call dbpinpol(xs(i), ys(i), in,dmiss, JINS, NPL, xpl, ypl, zpl)
      if ( in.eq.1) then
         imasks(i) = 1
      end if
   end do

   ierror = 0
1234 continue

!  restore polygon
   call restorepol()

   return
end subroutine disable_outside_curvigrid
