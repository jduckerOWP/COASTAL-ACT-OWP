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

! $Id: copygridtosam.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/copygridtosam.f90 $

!> Copy curvilinear grid to samples
 subroutine copygridtosam()
 use m_samples
 use m_grid
 USE M_MISSING
 implicit none
 integer :: in, k, m, n
 in = -1
 k  = MC*NC

 CALL INCREASESAM(k)

 K = 0
 MXSAM = MC
 MYSAM = NC
 xs = DMISS
 ys = DMISS
 zs = DMISS
 IPSTAT = IPSTAT_NOTOK
 do n=1,NC
    do m=1,MC
       k = k + 1
       if ( xc(m,n).ne.DMISS .and. yc(m,n).ne.DMISS ) then
         xs(k) = xc(m,n)
         ys(k) = yc(m,n)
         zs(k) = zc(m,n)
       end if
    end do
 enddo
 ns = k

 end subroutine copygridtosam
