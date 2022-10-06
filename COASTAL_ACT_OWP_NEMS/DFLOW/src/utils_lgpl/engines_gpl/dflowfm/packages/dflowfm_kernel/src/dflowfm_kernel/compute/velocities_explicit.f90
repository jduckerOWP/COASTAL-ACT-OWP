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

! $Id: velocities_explicit.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/velocities_explicit.f90 $

 subroutine velocities_explicit()
 use m_flowgeom
 use m_flow
 use m_flowtimes
 implicit none
 integer :: n, L, LL, k1, k2

 if (itstep == 1) then
    u1 = (u0 - dts*adve)/(1d0 + dts*advi)
    do n  = 1, nbndu  !       boundaries at u points
       L     = kbndu(3,n)
       u1(L) = zbndu(n)
    end do
 endif
 q1   = u1*au

 squ = 0d0 ; sqi = 0d0
 if ( kmx.eq.0 ) then
    do L = 1,lnx
      if (q1(L) > 0) then
          k1 = ln(1,L) ; k2 = ln(2,L)
          squ(k1) = squ(k1) + q1(L)
          sqi(k2) = sqi(k2) + q1(L)
       else if (q1(L) < 0) then
          k1 = ln(1,L) ; k2 = ln(2,L)
          squ(k2) = squ(k2) - q1(L)
          sqi(k1) = sqi(k1) - q1(L)
       endif
    enddo
 else
    do LL = 1,lnx
       do L=Lbot(LL),Ltop(LL)
          if (q1(L) > 0) then
             k1 = ln(1,L) ; k2 = ln(2,L)
             squ(k1) = squ(k1) + q1(L)
             sqi(k2) = sqi(k2) + q1(L)
          else if (q1(L) < 0) then
             k1 = ln(1,L) ; k2 = ln(2,L)
             squ(k2) = squ(k2) - q1(L)
             sqi(k1) = sqi(k1) - q1(L)
          endif
       end do
    enddo
 end if
 end subroutine velocities_explicit
