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

! $Id: getprof_1d_min.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/getprof_1d_min.f90 $

subroutine getprof_1D_min(L, hpr, area, width) ! pressurepipe
use m_profiles
use m_flow
use m_flowgeom
use unstruc_channel_flow
use m_crosssections
use m_cross_helper


implicit none
integer          :: L
double precision :: hpr                  ! hoogte in profiel
double precision :: area                 ! wet cross sectional area
double precision :: width                ! width at water surface

double precision :: profw                ! width  of profile
double precision :: profh                ! height of profile
double precision :: area2, width2        ! second prof i.c. interpolation
double precision :: alfa, hh
integer          :: LL, ka, kb, itp

area = 0d0 ; width = 0d0

LL = L
if (L > lnxi) then                       ! for 1D boundary links, refer to attached link
   LL = LBND1D(L)
endif


if (abs(kcu(ll))==1 .and. network%loaded) then !flow1d used only for 1d channels and not for 1d2d roofs and gullies
   call GetCSParsTotal(network%adm%line2cross(LL, 2), network%crs%cross, hpr, area, width, CS_TYPE_MIN)
   return
endif

if (prof1D(1,LL) >= 0 ) then              ! direct profile based upon link value
    ka    = 0; kb = 0                    ! do not use profiles
    profw = prof1D(1,LL)
    profh = prof1D(2,LL)
    itp   = prof1D(3,LL)
else
    ka    = -prof1D(1,LL); kb = -prof1D(2,LL)
    profw = profiles1D(ka)%width
    profh = profiles1D(ka)%height
    itp   = profiles1D(ka)%ityp
endif

! negative = closed
if (itp  == -1) then                      ! pipe
    call pipemin(hpr, profw, area, width)
else if (itp < 0) then                    ! closed rest
   hh = hpr - profh
   if (hh > 0d0) then
       width = profw
       area  = hh*width
   endif
endif

if (ka .ne. 0 .and. kb .ne. ka) then     ! interpolate in profiles
    area2 = 0d0 ; width2 = 0d0
    profw = profiles1D(kb)%width
    profh = profiles1D(kb)%height
    itp   = profiles1D(kb)%ityp
    alfa  = prof1d(3,LL)
    if (itp  == -1) then                       ! pipe
       call pipemin(hpr, profw, area2, width2)
    else                                       ! rest
       hh = hpr - profh
       if (hh > 0d0) then
           width2 = profw
           area2  = hh*width2
       endif
    endif
    area  = (1d0-alfa)*area  + alfa*area2
    width = (1d0-alfa)*width + alfa*width2
endif
end subroutine getprof_1D_min
