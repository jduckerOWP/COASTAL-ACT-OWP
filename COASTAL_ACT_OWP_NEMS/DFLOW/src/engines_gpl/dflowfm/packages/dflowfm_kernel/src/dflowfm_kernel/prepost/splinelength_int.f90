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

! $Id: splinelength_int.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/splinelength_int.f90 $

!> approximate spline pathlength in interval
double precision function splinelength_int(num, xspl, yspl, s0, s1)

   use geometry_module, only: dbdistance
   use m_missing, only: dmiss
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   integer,                          intent(in) :: num          !< number of spline control points
   double precision, dimension(num), intent(in) :: xspl, yspl   !< coordinates of slpine control points
   double precision,                 intent(in) :: s0, s1       !< begin and end of interval in spline coordinates respectively

   double precision, dimension(num)             :: xspl2, yspl2 !  second order derivates of spline coordinates

   double precision                             :: xL, yL, xR, yR, tL, tR, dt, fac

   integer                                      :: i,j,N

   integer, parameter                           :: NSAM = 100   ! sample factor
   integer, parameter                           :: Nmin = 10    ! minimum number of intervals

   call splinxy(xspl, yspl, xspl2, yspl2, num)


   dt = 1d0/dble(NSAM)
!  number of intervals
   N = max(floor(0.9999d0+(s1-s0)/dt), Nmin)
   dt = (s1-s0)/dble(N)

!   tR = s0
!   call splintxy(xspl,yspl,xspl2,yspl2,num,tR,xR,yR)

   splinelength_int = 0d0
   tR = s0
   call splintxy(xspl,yspl,xspl2,yspl2,num,tR,xR,yR)
   do i=1,N
      tL = tR
      xL = xR
      yL = yR
      fac = dble(i)/dble(N)
      tR = (1d0-fac) * s0 + fac*s1
      call splintxy(xspl,yspl,xspl2,yspl2,num,tR,xR,yR)
      splinelength_int = splinelength_int + dbdistance(xL,yL,xR,yR, jsferic, jasfer3D, dmiss)
   end do

   return
end function splinelength_int
