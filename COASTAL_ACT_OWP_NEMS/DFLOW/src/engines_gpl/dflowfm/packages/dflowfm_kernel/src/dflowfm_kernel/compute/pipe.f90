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

! $Id: pipe.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/pipe.f90 $

subroutine pipe(hpr, dia, area, width, japerim, perim) ! half open part
use m_sferic
use m_flow, only : slotw1D
!
! this subroutine computes wetted circle surface as function
! of diameter d and waterdepth dpt, as an option (if jd=1) it can compute
! the derivative da(dpt)/ddpt and (if jw=1) it can also compute the wetted
! perimeter
!
!  dpt   I, water depth
!  dia   I, diameter
!  wet   O, wetted surface
!  dwdd  O, det/ddpt
!  wtp   O, wetted perimeter
!  jd    I, compute dwdd if jd=1
!  jw    I, compute wtp if jw=1
!  sl    I, slotbreedte
implicit none
integer, intent(in)            :: japerim
double precision, intent(in)   :: dia, hpr
double precision, intent(out)  :: area, width, perim

! Local variables

double precision               :: are, dacos, dsqrt, fi, r, sq

r    = 0.5*dia
are  = r - hpr
if (hpr< r) then
   fi = dacos(are/r)
   sq = dsqrt(hpr*(dia - hpr))
   area  = fi*r*r - sq*are
   width = 2*sq
   if (japerim == 1) perim = 2*fi*r
else
   area  = 0.5d0*pi*r*r+(hpr-r)*dia
   width = dia
   if (japerim == 1) then
       if (hpr < dia) then
          fi = dacos(are/r)
          sq = dsqrt(hpr*(dia - hpr))
          area  = fi*r*r - sq*are
          perim = 2*fi*r
       else
          area  = pi*r*r
          perim = twopi*r
       endif
   endif
endif
if (slotw1D > 0 .and. japerim == 0) then
   width = width + slotw1D
   area  = area  + slotw1D*hpr
endif
end subroutine pipe
