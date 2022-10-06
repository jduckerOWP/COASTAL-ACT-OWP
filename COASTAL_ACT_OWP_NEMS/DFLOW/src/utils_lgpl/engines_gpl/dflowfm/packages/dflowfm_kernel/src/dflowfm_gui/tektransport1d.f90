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

! $Id: tektransport1d.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/tektransport1d.f90 $

 subroutine tektransport1D(tim)
 use m_sferic
 use m_statistics
 use m_flowgeom
 use m_flow
 use m_transport
 implicit none
 double precision :: tim
 double precision :: cwave, period, omeg, wlen, rk, phi, xx, yy, dif
 integer          :: k

 cwave   = 60d0*sqrt(10d0*1d-4)                ! chezy
 period = 90d0*60d0
 omeg = twopi/period     ! s
 wlen   = cwave*period
 rk     = twopi/wlen
 do k   = 1,600
    xx  = -50d0 + (k-1)*100d0
    phi = rk*xx - omeg*tim
    yy  = 15d0 + 10d0*cos(phi)
    if (k == 1) then
       call movabs(xx,yy)
    else
       call  lnabs(xx,yy)
    endif
 enddo

 if (ndxi < 1) return

 avedif = 0d0
 do k = 1,ndxi
    xx  = xz(k)
    phi = rk*xx - omeg*tim
    yy  = 15d0 + 10d0*cos(phi)
    dif = abs(constituents(isalt,k) - yy)
    avedif = avedif + dif
 enddo
 avedif = avedif/ndxi

 end subroutine tektransport1D
