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

! $Id: netmodfld.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/netmodfld.f90 $

!> network field move
!!   Is is assumed that there is a backup copy of the grid.
subroutine netmodfld(xp,yp,zp,kp)
 use m_netw
 use m_grid
 use m_alloc
 use m_missing
 use m_wearelt
 use m_sferic
 use geometry_module, only: dbdistance

 implicit none

 double precision           :: xp, yp, zp     !< coordinates that determine the influenced region

 integer                    :: kp             !< center point index

 double precision           :: Dx0, Dy0, rsx, xn, yn, dist, frac
 double precision           :: xcen, ycen
 double precision, external :: getDx, getDy

 integer                    :: i

 xcen = xk(kp)
 ycen = yk(kp)

 Dx0 = xp - xcen
 Dy0 = yp - ycen

 rsx = max(dsix, sqrt(Dx0*Dx0 + Dy0*Dy0))

 do i=1,numk
    xn     = xk(i)
    yn     = yk(i)
! intentional not in sferical coordinates
    dist  = sqrt( (xn-xcen)**2 + (yn-ycen)**2 )
    frac  = 0.5 * (1+ cos(min(max(dist/rsx,-1d0),1d0) * pi))

    xk(i) = xk(i) + Dx0*frac
    yk(i) = yk(i) + Dy0*frac
 end do


end subroutine netmodfld
