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

! $Id: highlight_nodesnlinks.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/highlight_nodesnlinks.f90 $

!> Highlights net/flow nodes and/or links, when specified in display parameters.
subroutine highlight_nodesnlinks()
    use unstruc_display
    use unstruc_colors
    use network_data
    use m_flowgeom
    implicit none

    integer :: L

    ! if (jaHighlight /= 1) return

    if (nhlNetNode > 0 .and. nhlNetNode <= numk) then
        call cirr(xk(nhlNetNode), yk(nhlNetNode), ncolhl)
    end if

    if (nhlNetLink > 0 .and. nhlNetLink <= numl) then
        call cirr(.5d0*(xk(kn(1,nhlNetLink))+xk(kn(2,nhlNetLink))), &
                  .5d0*(yk(kn(1,nhlNetLink))+yk(kn(2,nhlNetLink))), ncolhl)
        call teklink(nhlNetLink, ncolhl)
    end if

    if (nhlFlowNode > 0 .and. nhlFlowNode <= ndx) then
       call cirr(xz(nhlFlowNode), yz(nhlFlowNode), ncolhl)
    end if

    if (nhlFlowLink > 0 .and. nhlFlowLink <= lnx) then
        call cirr(xu(nhlFlowLink), yu(nhlFlowLink), ncolhl)
    end if

end subroutine highlight_nodesnlinks
