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

! $Id: resetflow.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/resetflow.f90 $

 !> Resets the current flow- and time-state, but keeps al active parameter settings.
 !! To be called upon flow_modelinit().
 !! Upon program startup and loading of new model/MDU, call resetFullFlowModel() instead.
 subroutine resetFlow()
 use m_wind
 use m_flow
 use m_flowexternalforcings
 use m_flowparameters
 use m_statistics
 use m_flowgeom
 use m_flowtimes
 use waq
 use m_waves
 use m_hydrology_data
 use m_sobekdfm
 use m_save_ugrid_state, only: reset_save_ugrid_state
 use m_longculverts, only: reset_longculverts
 implicit none

    ! Only reset counters and other scalars, allocatables should be
    ! automatically reset elsewhere (e.g., allocateandset*, flow_geominit)

    call reset_wind()

    call reset_waves()

    call reset_sobekdfm()

    ! Reset some flow (rest is done in flow_geominit())
    call reset_flowgeom()

    call reset_flowexternalforcings()

    call reset_longculverts()

    call reset_flowtimes()

    ! call reset_flowparameters()

    call reset_flow()

    call reset_waq()

    call reset_movobs()

    call reset_statistics()

    if ( jawave.eq.4 ) then
       call xbeach_reset()
    end if

    call reset_save_ugrid_state()

    call reset_sedtra()

    call reset_hydrology_data()

 end subroutine resetFlow
