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

! $Id: setbobs_fixedweirs.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/setbobs_fixedweirs.f90 $

!> Sets the bob values on the flow links that are overridden by a fixed weir.
!! This is based on the interpolated pliz values from the fixed weir definition.
subroutine setbobs_fixedweirs()
use m_flowgeom
use m_fixedweirs
implicit none

integer                       :: i, ip, iL, Lf
double precision              :: alpha, zc


if ( nfxw == 0 ) return

do i = 1,nfxw
    do iL=1,fxw(i)%lnx
        Lf = abs(fxw(i)%ln(iL))
        ip = fxw(i)%indexp(iL)
        alpha = fxw(i)%wfp(iL)
        zc = alpha * fxw(i)%zp(ip) + (1d0-alpha)*fxw(i)%zp(ip+1)
        bob(1,Lf) = max( zc,bob(1,Lf) ) ; bob(2,Lf) = max( zc,bob(2,Lf) )
    end do
end do
end subroutine setbobs_fixedweirs
