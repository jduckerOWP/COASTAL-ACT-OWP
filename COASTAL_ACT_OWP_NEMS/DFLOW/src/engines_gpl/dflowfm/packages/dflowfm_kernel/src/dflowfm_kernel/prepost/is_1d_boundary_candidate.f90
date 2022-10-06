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

! $Id: is_1d_boundary_candidate.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/is_1d_boundary_candidate.f90 $

!< Returns true when a 1d node can be used as a boundary. By default
!< the connecting edge should not lead to a bifurcation, unless
!< the flag jaAllowBndAtBifurcation is true
pure logical function is_1d_boundary_candidate(L,i)
    use network_data
    use m_flowgeom

    implicit none

    integer, intent(in)     :: L    !<  net link to check for boundary candidate
    integer, intent(in)     :: i    !<  node to check, equals 1 or 2

    logical                 :: isEndNode

    is_1d_boundary_candidate = nmk(kn(i,L)) == 1 .and. nmk(kn(3-i,L)) == 2 .and. lne(i,L) < 0
    if (jaAllowBndAtBifurcation == 1) then
        is_1d_boundary_candidate = nmk(kn(i,L)) == 1 .and. nmk(kn(3-i,L)) >= 2 .and. lne(i,L) < 0
    endif

    return
end function is_1d_boundary_candidate
