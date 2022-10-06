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

! $Id: flow_obsinit.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/flow_obsinit.f90 $

 !> Initializes all administration encessary for writing output to his-files.
 !! That is: snap observation stations to flow cells, cross sections to flow links.
 !! And bookkeeping for time series output on structures.
 subroutine flow_obsinit()
 use m_observations, only: init_valobs
 use m_structures
 implicit none
    call crosssections_on_flowgeom()
    call runupgauges_on_flowgeom()

    call obs_on_flowgeom(0)

!   for the following, it is assumed that the moving observation stations have been initialized (in flow_initexternalforcings)
    call init_valobs()   ! (re)initialize work array and set pointers for observation stations

    call updateValuesOnObservationStations() ! and fill first value

    call init_structure_hisvalues()

 end subroutine flow_obsinit
