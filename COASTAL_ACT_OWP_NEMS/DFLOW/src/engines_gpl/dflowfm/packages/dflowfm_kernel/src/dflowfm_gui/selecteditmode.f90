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

! $Id: selecteditmode.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/selecteditmode.f90 $

    !> Selects the edit mode for a given keypress code.
    !! Alt-P/-N/-S/-G/-B/-F for the respective modes.
    subroutine selecteditmode(newmode, key)
    implicit none
    integer, intent(inout) :: newmode !< New mode (0 for invalid key presses).
    integer, intent(in)    :: key     !< Key press code

    if      (key == 512+80) then ! Alt+P: Edit Polygon
        newmode = 1
    else if (key == 512+78) then ! Alt+N: Edit Network
        newmode = 2
    else if (key == 512+83) then ! Alt+S: Edit Splines
        newmode = 3
    else if (key == 512+71) then ! Alt+G: Edit Grid
        newmode = 4
    else if (key == 512+66) then ! Alt+B: Edit Samples (bathymetry)
        newmode = 5
    else if (key == 512+70) then ! Alt+F: Edit Flow
        newmode = 6
    end if
    return
    end subroutine selecteditmode
