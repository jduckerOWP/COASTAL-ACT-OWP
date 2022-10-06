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

! $Id: unstruc_guimessage.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/unstruc_guimessage.f90 $

!----------------------------------------------------------------------
! subroutines from either net.F90 or rest.F90 that are still needed
!   without the GUI
!----------------------------------------------------------------------
!> Shows a message in a GUI dialog (Interacter only).
!! This routine is supposed to be called from the utility modules,
!! such as gridgeom, as a callback.
!!
!! NOTE: this subroutine is dflowfm's implementation of the MHCallBack::messagebox_iface interface.
subroutine unstruc_guimessage(title, msg, level)
    use unstruc_messages
    implicit none
    character(len=*)    :: title !< Title string
    character(len=*)    :: msg   !< Message string
    integer, intent(in) :: level !< Severity level, use values from the MessageHandling module (e.g., LEVEL_ERROR). Currently not used.

    call qnerror(msg, ' ', ' ')

end subroutine unstruc_guimessage
