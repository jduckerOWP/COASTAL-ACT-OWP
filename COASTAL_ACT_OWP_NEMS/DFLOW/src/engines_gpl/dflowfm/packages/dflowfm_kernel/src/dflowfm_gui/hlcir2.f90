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

! $Id: hlcir2.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/hlcir2.f90 $

      !> Draw a filled circle at current position.
      !! Filled means: one colour for inside, one colour for edge.
      subroutine HLCIR2(R, icolfill, icoledge)
      implicit none
        double precision, intent(in) :: R    !< Radius in world coords.
        integer,          intent(in) :: icolfill !< Colour number for inner fill
        integer,          intent(in) :: icoledge !< Colour number for edge

        CALL IGRFILLPATTERN(4,0,0)
        CALL SETCOL(icolfill)
        CALL CIR(R)
        CALL IGRFILLPATTERN(0,0,0)
        CALL SETCOL(icoledge)
        CALL CIR(R)
        CALL IGRFILLPATTERN(4,0,0)
      end subroutine HLCIR2
