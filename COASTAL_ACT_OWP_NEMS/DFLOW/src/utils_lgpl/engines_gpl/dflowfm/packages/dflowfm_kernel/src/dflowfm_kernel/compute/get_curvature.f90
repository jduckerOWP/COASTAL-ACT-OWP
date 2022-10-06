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

! $Id: get_curvature.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/get_curvature.f90 $

!    Secondary Flow
subroutine get_curvature         ! Find the curvature of the bend, to be used in secondary flow
    use m_flow
    use m_flowgeom
    use m_netw

    implicit none
    integer :: k, k1, k2, L, LL, n
    double precision :: cofa, cofb, cofc, cofd, cofe, coff, cofg, cofw, cofx, cofy, cofu, cofv, cof0
    double precision :: dudx, dudy, dvdx, dvdy

    do k = 1,ndx
       spirucm(k) = 0d0
       if( hs(k) < epshu ) cycle
       spirucm(k) = sqrt( ucx(k) * ucx(k) + ucy(k) * ucy(k) )
    enddo

    do k = 1,ndxi
       if( spirucm(k) < 1.0d-3 .or. hs(k) < epshu ) then
          spircrv(k) = 0.0d0
          cycle
       endif
       cofa = 0.0d0
       cofb = 0.0d0
       cofc = 0.0d0
       cofd = 0.0d0
       cofe = 0.0d0
       coff = 0.0d0
       cofg = 0.0d0
       n = 0
       do LL = 1,nd(k)%lnx
          L = abs( nd(k)%ln(LL) )
          k2 = ln(1,L) + ln(2,L) - k
          !if( hs(k2) < epshu ) cycle
          n = n + 1
          cofx = xz(k2) - xz(k)
          cofy = yz(k2) - yz(k)
          cofu = ucx(k2) - ucx(k)
          cofv = ucy(k2) - ucy(k)
          if( hs(k2) < epshu ) then
             cofu = - ucx(k)
             cofv = - ucy(k)
          endif
          cof0 = sqrt( cofx * cofx + cofy * cofy )
          cofw = 1.0d0 / cof0
          if( cof0 < 1.0d-6 ) cofw = 1.0d6
          cofx = cofw * cofx
          cofy = cofw * cofy
          cofu = cofw * cofu
          cofv = cofw * cofv
          cofa = cofa + cofx * cofx
          cofb = cofb + cofx * cofy
          cofc = cofc + cofy * cofy
          cofd = cofd + cofu * cofx
          cofe = cofe + cofu * cofy
          coff = coff + cofv * cofx
          cofg = cofg + cofv * cofy
       enddo
       cof0 = cofa * cofc - cofb * cofb
       spircrv(k) = 0.0d0
       if( cof0 < 1d-6 .or. n < 2 ) cycle
       dudx = ( cofd * cofc - cofb * cofe ) / cof0
       dudy = ( cofa * cofe - cofd * cofb ) / cof0
       dvdx = ( coff * cofc - cofb * cofg ) / cof0
       dvdy = ( cofa * cofg - coff * cofb ) / cof0
       spircrv(k) = ucx(k) * ucx(k) * dvdx - ucy(k) * ucy(k) * dudy + ucx(k) * ucy(k) * ( dvdy - dudx )
       spircrv(k) = - spircrv(k) / spirucm(k)**3
    enddo

    do L = lnxi+1,lnx                     ! Boundary condtions as Neumann for the curvature
       k1 = ln(1,L) ; k2 = ln(2,L)
       spircrv(k1)  = spircrv(k2)
    enddo

end subroutine get_curvature
