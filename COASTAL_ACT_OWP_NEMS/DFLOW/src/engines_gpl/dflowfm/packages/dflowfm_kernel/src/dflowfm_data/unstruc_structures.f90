module m_structures

!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2022.
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
   
! $Id: unstruc_structures.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_data/unstruc_structures.f90 $

use properties
use unstruc_channel_flow, only: network
use MessageHandling
implicit none

type(tree_data), pointer, public :: strs_ptr !< A property list with all input structure specifications of the current model. Not the actual structure set.
integer :: jaoldstr !< tmp backwards comp: we cannot mix structures from EXT and from structure-input files. Use one or the other.

 ! Structure Parameters
 double precision, dimension(:,:), allocatable :: valpump     !< Array for pump;      (1,:) flow link width, used for averaging.
                                                              !<                      (2,:) pump discharge w.r.t. structure orientation
                                                              !<                      (3,:) pump structure water level up
                                                              !<                      (4,:) pump structure water level down
                                                              !<                      (5,:) pump structure head
                                                              !<                      (6,:) pump capacity
                                                              !<                      (7,:) actual pump stage
                                                              !<                      (8,:) pump head
                                                              !<                      (9,:) pump reduction factor
                                                              !<                      (10,:) pump water level at delivery side
                                                              !<                      (11,:) pump water level at suction side
                                                              !<                      (12,:) pump discharge w.r.t. pumping orientation (same sign as capacity)

 double precision, dimension(:,:), allocatable :: valgate     !< Array for gate;      (1,:) discharge through gate
 double precision, dimension(:,:), allocatable :: valcdam     !< Array for cdam;      (1,:) discharge through controlable dam
                                                              !<                      (2,:) Upstream average water levels
                                                              !<                      (3,:) downstream average water level
                                                              !<                      (4,0) width of dam
 double precision, dimension(:,:), allocatable :: valgategen  !< Array for gate(new), (1,:) discharge through gate
                                                              !<                      (2,:) Upstream average water level
                                                              !<                      (3,:) gate width
 double precision, dimension(:,:), allocatable :: valweirgen  !< Array for weir;      (1,:) flow link width, used for averaging.
                                                              !<                      (2,:) discharge through weir
                                                              !<                      (3,:) weir structure water level up
                                                              !<                      (4,:) weir structure water level down
                                                              !<                      (5,:) weir structure head
                                                              !<                      (6,:) weir flow area
                                                              !<                      (7,:) weir velocity
                                                              !<                      (8,:) water level on crest
                                                              !<                      (9,:) weir crest level
                                                              !<                      (10,:) weir crest width
                                                              !<                      (11,:) weir state (0: closed, 1: free weir, 2: drowned/submerged weir)
                                                              !<                      (12,:) weir force difference per unit width
                                                              !<                      (13,:) weir counters of partitions for parallel
 double precision, dimension(:,:), allocatable :: valcgen     !< Array for general structure (old ext), (1,:) discharge
 double precision, dimension(:,:), allocatable :: valgenstru  !< Array for general structure (1,:) flow link width, used for averaging.
                                                              !<                      (2,:) discharge through general structure
                                                              !<                      (3,:) general structure water level up
                                                              !<                      (4,:) general structure water level down
                                                              !<                      (5,:) general structure head
                                                              !<                      (6,:) general structure flow area
                                                              !<                      (7,:) general structure velocity
                                                              !<                      (8,:) general structure water level on crest
                                                              !<                      (9,:) general structure crest level
                                                              !<                      (10,:) general structure crest width
                                                              !<                      (11,:) general structure state (0: closed, 1: free weir, 2: drowned/submerged weir)
                                                              !<                      (12,:) general structure force difference per unit width
                                                              !<                      (13,:) general structure gate opening width
                                                              !<                      (14,:) general structure gate lower edge level
                                                              !<                      (15,:) general structure gate opening height
                                                              !<                      (16,:) general structure gate upper edge level
                                                              !<                      (17,:) general structure discharge through gate opening
                                                              !<                      (18,:) general structure discharge over gate
                                                              !<                      (19,:) general structure discharge under gate
                                                              !<                      (20,:) general structure flow area in gate opening
                                                              !<                      (21,:) general structure flow area over gate
                                                              !<                      (22,:) general structure flow area under gate
                                                              !<                      (23,:) general structure velocity through gate opening
                                                              !<                      (24,:) general structure velocity over gate
                                                              !<                      (25,:) general structure velocity under gate
                                                              !<                      (26,:) general structure counters of partitions for parallel
 double precision, dimension(:,:), allocatable, target :: valdambreak !< Array for dambreak, (1,:)  flow link width
                                                              !<                      (2,:) instantanuous discharge
                                                              !<                      (3,:) dambreak water level up
                                                              !<                      (4,:) dambreak water level down
                                                              !<                      (5,:) dambreak structure head
                                                              !<                      (6,:) dambreak flow area
                                                              !<                      (7,:) dambreak normal velocity
                                                              !<                      (8,:) dambreak crest level
                                                              !<                      (9,:) dambreak crest width
                                                              !<                      (10,:) dambreak water level jump
                                                              !<                      (11,:) dambreak breach width time derivative
                                                              !<                      (12,:) cumulative discharge
 double precision, dimension(:,:), allocatable :: valorifgen  !< Array for orifice (1,:) flow link width, used for averaging.
                                                              !<                      (2,:) discharge through orifice
                                                              !<                      (3,:) orifice water level up
                                                              !<                      (4,:) orifice water level down
                                                              !<                      (5,:) orifice head
                                                              !<                      (6,:) orifice flow area
                                                              !<                      (7,:) orifice velocity
                                                              !<                      (8,:) orifice water level on crest
                                                              !<                      (9,:) orifice crest level
                                                              !<                      (10,:) orifice crest width
                                                              !<                      (11,:) orifice state (0: closed, 1: free weir, 2: drowned/submerged weir)
                                                              !<                      (12,:) orifice force difference per unit width
                                                              !<                      (13,:) orifice gate opening width (not applicable)
                                                              !<                      (14,:) orifice gate lower edge level
                                                              !<                      (15,:) orifice gate opening height
                                                              !<                      (16,:) orifice counters of partitions for parallel
 double precision, dimension(:,:), allocatable :: valbridge   !< Array for bridge;    (1,:) flow link width, used for averaging.
                                                              !<                      (2,:) discharge through bridge
                                                              !<                      (3,:) bridge water level up
                                                              !<                      (4,:) bridge water level down
                                                              !<                      (5,:) bridge head
                                                              !<                      (6,:) bridge flow area
                                                              !<                      (7,:) bridge velocity
                                                              !<                      (8,:) bridge bed level up
                                                              !<                      (9,:) bridge bed level down
                                                              !<                      (10,:) bridge actual bed level (crest)
 double precision, dimension(:,:), allocatable :: valculvert  !< Array for culvert;   (1,:) flow link width, used for averaging.
                                                              !<                      (2,:) discharge through culvert
                                                              !<                      (3,:) culvert water level up
                                                              !<                      (4,:) culvert water level down
                                                              !<                      (5,:) culvert structure head
                                                              !<                      (6,:) culvert flow area
                                                              !<                      (7,:) culvert velocity
                                                              !<                      (8,:) culvert crest level
                                                              !<                      (9,:) culvert state (0: closed, 1: free weir, 2: drowned/submerged weir)
                                                              !<                      (10,:) culvert gate lower edge level
                                                              !<                      (11,:) culvert gate opening height
 double precision, dimension(:,:), allocatable :: valuniweir  !< Array for universal weir; (1,:) flow link width, used for averaging.
                                                              !<                      (2,:) discharge through universal weir
                                                              !<                      (3,:) universal weir water level up
                                                              !<                      (4,:) universal weir water level down
                                                              !<                      (5,:) universal weir head
                                                              !<                      (6,:) universal weir flow area
                                                              !<                      (7,:) universal weir velocity
                                                              !<                      (8,:) universal weir crest level
 double precision, dimension(:,:), allocatable :: valcmpstru  !< Array for compound structure; (1,:) flow link width, used for averaging.
                                                              !<                      (2,:) discharge through compound structure
                                                              !<                      (3,:) compound structure water level up
                                                              !<                      (4,:) compound structure water level down
                                                              !<                      (5,:) compound structure head
                                                              !<                      (6,:) compound structure flow area
                                                              !<                      (7,:) compound structure velocity
 double precision, dimension(:,:), allocatable :: vallongculvert!< Array for long culvert, (1,:) flow link width, used for averaging.
                                                              !<                      (2,:) discharge through long culvert
                                                              !<                      (3,:) long culvert water level up
                                                              !<                      (4,:) long culvert water level down
                                                              !<                      (5,:) long culvert structure head
                                                              !<                      (6,:) long culvert flow area
                                                              !<                      (7,:) long culvert velocity
                                                              !<                      (8,:) long culvert valve relative opening
                                                              !<                      (9,:) TODO: UNST-4644:long culvert counters of partitions for parallel

 integer                           :: NUMVALS_PUMP = 12       !< Number of variables for pump
 integer                           :: NUMVALS_GATE = 5        !< Number of variables for gate
 integer                           :: NUMVALS_CDAM = 4        !< Number of variables for controble dam
 integer                           :: NUMVALS_CGEN = 4        !< Number of variables for general structure (old ext file)
 integer                           :: NUMVALS_GATEGEN = 9     !< Number of variables for gate (new)
 integer                           :: NUMVALS_WEIRGEN = 13    !< Number of variables for weir
 integer                           :: NUMVALS_GENSTRU = 26    !< Number of variables for general structure( new exe file)
 integer                           :: NUMVALS_DAMBREAK = 12   !< Number of variables for dambreak
 integer                           :: NUMVALS_ORIFGEN = 16    !< Number of variables for orific
 integer                           :: NUMVALS_BRIDGE  = 10    !< Number of variables for bridge
 integer                           :: NUMVALS_CULVERT = 11    !< Number of variables for culvert
 integer                           :: NUMVALS_UNIWEIR = 8     !< Number of variables for univeral weir
 integer                           :: NUMVALS_CMPSTRU = 7     !< Number of variables for compound structure
 integer                           :: NUMVALS_LONGCULVERT = 8 !< Number of variables for long culvert, TODO:UNST-4644: for parallel, the value should be 9
 
 integer                           :: jahiscgen               !< Write structure parameters to his file, 0: n0, 1: yes
 integer                           :: jahispump               !< Write pump      parameters to his file, 0: n0, 1: yes
 integer                           :: jahisgate               !< Write gate      parameters to his file, 0: n0, 1: yes
 integer                           :: jahiscdam               !< Write dam       parameters to his file, 0: n0, 1: yes
 integer                           :: jahisweir               !< Write weir      parameters to his file, 0: n0, 1: yes
 integer                           :: jahisdambreak           !< Write dambreak  parameters to his file, 0: n0, 1: yes
 integer                           :: jahisorif               !< Write orifice   parameters to his file, 0: no, 1: yes
 integer                           :: jahisbridge             !< Write bridge    parameters to his file, 0: no, 1: yes
 integer                           :: jahisculv               !< Write culvert   parameters to his file, 0: no, 1: yes
 integer                           :: jahisuniweir            !< Write univeral weir parameters to his file, 0: no, 1: yes
 integer                           :: jahiscmpstru            !< Write compound structure parameters to his file, 0: no, 1: yes
 integer                           :: jahislongculv           !< Write long culverts parameters to his file, 0: no, 1:yes
 
 !! Geometry variables
 ! weir
 integer                               :: nNodesWeir           !< [-] Total number of nodes for all weirs
 integer,          allocatable, target :: nodeCountWeir(:)     !< [-] Count of nodes per weir.
 double precision, allocatable, target :: geomXWeir(:)         !< [m] x coordinates of weirs.
 double precision, allocatable, target :: geomYWeir(:)         !< [m] y coordinates of weirs.
 ! general structure
 integer                               :: nNodesGenstru        !< [-] Total number of nodes for all general structures
 integer,          allocatable, target :: nodeCountGenstru(:)  !< [-] Count of nodes per general structure.
 double precision, allocatable, target :: geomXGenstru(:)      !< [m] x coordinates of general structures.
 double precision, allocatable, target :: geomYGenstru(:)      !< [m] y coordinates of general structures.
 ! orifice
 integer                               :: nNodesOrif           !< [-] Total number of nodes for all orifices
 integer,          allocatable, target :: nodeCountOrif(:)     !< [-] Count of nodes per orifice.
 double precision, allocatable, target :: geomXOrif(:)         !< [m] x coordinates of orifices.
 double precision, allocatable, target :: geomYOrif(:)         !< [m] y coordinates of orifices.
 ! universal weir
 integer                               :: nNodesUniweir        !< [-] Total number of nodes for all universal weirs
 integer,          allocatable, target :: nodeCountUniweir(:)  !< [-] Count of nodes per universal weir.
 double precision, allocatable, target :: geomXUniweir(:)      !< [m] x coordinates of universal weirs.
 double precision, allocatable, target :: geomYUniweir(:)      !< [m] y coordinates of universal weirs.
 ! culvert
 integer                               :: nNodesCulv           !< [-] Total number of nodes for all culverts
 integer,          allocatable, target :: nodeCountCulv(:)     !< [-] Count of nodes per culvert.
 double precision, allocatable, target :: geomXCulv(:)         !< [m] x coordinates of culverts.
 double precision, allocatable, target :: geomYCulv(:)         !< [m] y coordinates of culverts.
 ! pump
 integer                               :: nNodesPump           !< [-] Total number of nodes for all pumps
 integer,          allocatable, target :: nodeCountPump(:)     !< [-] Count of nodes per pump.
 double precision, allocatable, target :: geomXPump(:)         !< [m] x coordinates of pumps.
 double precision, allocatable, target :: geomYPump(:)         !< [m] y coordinates of pumps.
 ! bridge
 integer                               :: nNodesBridge         !< [-] Total number of nodes for all bridges
 integer,          allocatable, target :: nodeCountBridge(:)   !< [-] Count of nodes per bridge.
 double precision, allocatable, target :: geomXBridge(:)       !< [m] x coordinates of bridges.
 double precision, allocatable, target :: geomYBridge(:)       !< [m] y coordinates of bridges.
 
 integer, parameter :: IOPENDIR_FROMLEFT  = -1 !< Gate door opens/closes from left side.
 integer, parameter :: IOPENDIR_FROMRIGHT =  1 !< Gate door opens/closes from right side.
 integer, parameter :: IOPENDIR_SYMMETRIC =  0 !< Gate door opens/closes symmetrically (from center).

 type tgate                                          !< Gate structure type, before it gets evaluated as a general structure.
    !double precision :: sill_level       !< Not used: stored in zcgen(1,igen)
    !double precision :: lower_edge_level !< Not used: stored in zcgen(2,igen)
    !double precision :: opening_width    !< Not used: stored in zcgen(3,igen)
    double precision :: door_height       !< Height of the door, used for 'double-barrier' overflow. Time-INDEPENDENT.
    double precision :: sill_width        !< Width of the sill, may be larger than the opening width, such that in open part we have weir flow and in closed part we have gate flow. Time-INDEPENDENT.
    integer          :: opening_direction !< Direction from which the gate opens/closes, IOPENDIR_FROMLEFT|FROMRIGHT|SYMMETRIC.
 end type tgate

 ! TIDAL TURBINES: Insert allocatable of type structure_turbines here

 type(tgate), allocatable :: gates(:)
   contains


   !> Allocates and initializes all "valstruct"(:,:) arrays.
   !! Used for history output and/or restart file output for hydraulic structures.
   subroutine init_structure_hisvalues()
      use m_flowexternalforcings , only: npumpsg, ncgensg, ngatesg, ncdamsg, ngategen, ngenstru, nweirgen, ndambreaksg
      !use m_structures, only: NUMVALS_PUMP, NUMVALS_GATE, NUMVALS_CDAM, NUMVALS_CGEN, &
      !                        NUMVALS_GATEGEN, NUMVALS_WEIRGEN, NUMVALS_GENSTRU
      use m_alloc
      use m_flowtimes, only: ti_rst
      use m_longculverts, only: nlongculvertsg
      implicit none

      if((ti_rst > 0 .or. jahispump > 0) .and. npumpsg > 0) then
         if( allocated( valpump ) ) deallocate( valpump )
         allocate( valpump(NUMVALS_PUMP,npumpsg) ) ; valpump = 0d0
      endif
      if(ti_rst > 0 .or. jahiscgen > 0 ) then
         if( ncgensg > 0 ) then
            if( allocated( valcgen ) ) deallocate( valcgen )
            allocate( valcgen(NUMVALS_CGEN,ncgensg) ) ; valcgen = 0d0
         endif
         
         if (ngenstru == 0) then ! If it is new general structure, then it is stored in the network type
            ngenstru = network%sts%numGeneralStructures
         end if
         if( ngenstru > 0 ) then
            if( allocated( valgenstru ) ) deallocate( valgenstru )
            allocate( valgenstru(NUMVALS_GENSTRU,ngenstru) ) ; valgenstru  = 0d0
         endif
      endif
      if( jahisgate > 0 ) then
         if( ngatesg > 0 ) then
            if( allocated( valgate ) ) deallocate( valgate )
            allocate( valgate(NUMVALS_CGEN,ngatesg) ) ; valgate = 0d0
         endif
         if( ngategen > 0 ) then
            if( allocated( valgategen ) ) deallocate( valgategen )
            allocate( valgategen(NUMVALS_GATEGEN,ngategen) ) ; valgategen = 0d0
         endif
      endif
      if( jahiscdam > 0 .and. ncdamsg > 0) then
         if( allocated( valcdam) ) deallocate( valcdam )
         allocate( valcdam(NUMVALS_CDAM,ncdamsg) ) ; valcdam = 0d0
      endif
      if (nweirgen == 0) then ! If it is new 1D weir, the weir is stored in the network type
         nweirgen = network%sts%numWeirs
      end if
      
      if((ti_rst > 0 .or. jahisweir > 0) .and. nweirgen > 0) then
         if( allocated( valweirgen) ) deallocate( valweirgen )
         allocate( valweirgen(NUMVALS_WEIRGEN,nweirgen) ) ; valweirgen = 0d0
      endif
      if( jahisdambreak > 0 .and. ndambreaksg > 0) then
         if( allocated( valdambreak ) ) deallocate( valdambreak )
         allocate( valdambreak(NUMVALS_DAMBREAK,ndambreaksg) ) ; valdambreak = 0d0
      endif
      if((ti_rst > 0 .or. jahisorif > 0) .and. network%sts%numOrifices > 0) then
         if( allocated( valorifgen) ) deallocate( valorifgen )
         allocate( valorifgen(NUMVALS_ORIFGEN,network%sts%numOrifices) ) ; valorifgen = 0d0
      endif
      if( jahisbridge > 0 .and. network%sts%numBridges > 0) then
         if( allocated( valbridge) ) deallocate( valbridge )
         allocate( valbridge(NUMVALS_BRIDGE,network%sts%numBridges) ) ; valbridge = 0d0
      endif
      if( (ti_rst > 0 .or. jahisculv > 0) .and. network%sts%numCulverts > 0) then
         if( allocated( valculvert) ) deallocate( valculvert )
         allocate( valculvert(NUMVALS_CULVERT,network%sts%numCulverts) ) ; valculvert = 0d0
      endif
      if( jahisuniweir > 0 .and. network%sts%numUniWeirs > 0) then
         if( allocated( valuniweir ) ) deallocate( valuniweir )
         allocate( valuniweir(NUMVALS_UNIWEIR,network%sts%numUniWeirs) ) ; valuniweir = 0d0
      endif
      if( jahiscmpstru > 0 .and. network%cmps%count > 0) then
         if( allocated( valcmpstru ) ) deallocate( valcmpstru )
         allocate( valcmpstru(NUMVALS_CMPSTRU,network%cmps%count) ) ; valcmpstru = 0d0
      endif
      if( jahislongculv > 0 .and. nlongculvertsg > 0) then
         if( allocated( vallongculvert) ) deallocate( vallongculvert )
         allocate( vallongculvert(NUMVALS_LONGCULVERT,nlongculvertsg) ) ; vallongculvert = 0d0
      endif

! TIDAL TURBINES: Insert init_turbines here

   end subroutine init_structure_hisvalues

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_structures() instead.
subroutine default_structures()

call tree_destroy(strs_ptr)

call reset_structures()

! TIDAL TURBINES: Insert calls to deallocate_turbines and init_turbines here

   ! default settings for structure output to history file
   jahiscgen = 1
   jahispump = 1
   jahisgate = 1
   jahiscdam = 1
   jahisweir = 1
   jahisorif = 1
   jahisculv = 1
   jahisbridge   = 1
   jahisdambreak = 1
   jahisuniweir = 1
   jahiscmpstru = 1
   jahislongculv = 1

end subroutine default_structures


!> Resets only structures variables intended for a restart of an existing flow simulation (same MDU).
!! Upon loading of new model/MDU, call default_structures() instead.
subroutine reset_structures()
   if (allocated(gates)) deallocate(gates)
end subroutine reset_structures

!> Fills the valstruct array for one given structure on a given link LL.
!! All values are filled, both the generic ones, as well as the type-specific ones.
!! Note: old-style structures may call this with istrtypein = ST_UNSET.
subroutine fill_valstruct_perlink(valstruct, L, dir, istrtypein, istru, L0)
   use m_missing, only: dmiss
   use m_flow, only: q1, s1, au, hu
   use m_flowgeom, only: wu, ln, teta, bl
   use m_1d_structures, only: get_discharge_under_compound_struc
   use m_General_Structure
   use m_GlobalParameters
   use m_longculverts
   implicit none
   double precision, dimension(:), intent(inout) :: valstruct   !< Output values on structure (e.g. valweirgen(:)):
                                                                !< (1) total width
                                                                !< (2) structure discharge
                                                                !< (3) structure water level up
                                                                !< (4) structure water level down
                                                                !< (5) structure head
                                                                !< (6) flow area
                                                                !< (7) velocity
                                                                !< (8) water level on crest, or valve relative opening if type is long culvert
                                                                !< (9) crest level
                                                                !< (10) crest width
                                                                !< (11) state
                                                                !< (12) force difference per unit width
                                                                !< (13) gate opening width
                                                                !< (14) gate lower edge level
                                                                !< (15) gate opening height
                                                                !< (16) gate upper edge level
                                                                !< (17) discharge through gate opening
                                                                !< (18) discharge over gate
                                                                !< (19) discharge under gate
                                                                !< (20) flow area in gate opening
                                                                !< (21) flow area over gate
                                                                !< (22) flow area under gate
                                                                !< (23) velocity through gate opening
                                                                !< (24) velocity over gate
                                                                !< (25) velocity under gate
   integer,                        intent(in   ) :: L           !< Flow link number.
   double precision,               intent(in   ) :: dir         !< Direction of flow link w.r.t. structure orientation (1.0 for same direction, -1.0 for opposite).
   integer,                        intent(in   ) :: istrtypein  !< The type of the structure. May differ from the struct%type, for example:
                                                                !< an orifice should be called with istrtypein = ST_ORIFICE, whereas its struct(istru)%type = ST_GENERAL_ST.
   integer,                        intent(in   ) :: istru       !< Structure index in network%sts set or in longculverts.
   integer,                        intent(in   ) :: L0          !< Local flow link index in the struct%linknumbers array.

   integer :: ku, kd, k1, k2
   type(t_GeneralStructure), pointer :: genstr
   double precision :: qcmp

   if (istrtypein == ST_LONGCULVERT) then
      ku = longculverts(istru)%flownode_up
      kd = longculverts(istru)%flownode_dn
   else
      if (dir > 0) then
         ku = ln(1,L)
         kd = ln(2,L)
      else
         ku = ln(2,L)
         kd = ln(1,L)
      end if
   end if

   ! 1. Generic values that apply to all structure types
   valstruct(1) = valstruct(1) + wu(L)
   
   if (istru > 0) then ! When it is not old weir and not old general structure and not a compound structure
      if (network%sts%struct(istru)%compound > 0) then ! for a structure that belongs to a compound structure
         k1 = ln(1,L)
         k2 = ln(2,L)
         if (hu(L) > 0) then
            qcmp = get_discharge_under_compound_struc(network%sts%struct(istru), L0, s1(k1), s1(k2), teta(L))
         else
            qcmp = 0d0
         end if
         valstruct(2) = valstruct(2) + qcmp*dir
      else
         valstruct(2) = valstruct(2) + q1(L)*dir
      end if
   else
      valstruct(2) = valstruct(2) + q1(L)*dir
   end if
   
   valstruct(3) = valstruct(3) + s1(ku)*wu(L)
   valstruct(4) = valstruct(4) + s1(kd)*wu(L)
   valstruct(5) = valstruct(5) + (s1(ku) - s1(kd))*wu(L)

   if (istrtypein /= ST_PUMP) then ! Compute flow area for structures except for pump
      if (istru > 0) then ! When it is not old weir and not old general structure and not a compound structure
         if (network%sts%struct(istru)%compound > 0) then ! for a structure that belongs to a compound structure
            valstruct(6) = valstruct(6) + network%sts%struct(istru)%au(L0)
         else
            valstruct(6) = valstruct(6) + au(L)
         end if
      else
         valstruct(6) = valstruct(6) + au(L)
      end if
   end if

   ! 2. More specific valus that apply to certain structure types only

   ! 2a. General structure-based structures with a crest.
   if (any(istrtypein == (/ ST_GENERAL_ST, ST_WEIR, ST_ORIFICE /))) then ! TODO: ST_GATE
      valstruct(8)  = valstruct(8) + network%sts%struct(istru)%generalst%sOnCrest(L0)*wu(L)
      valstruct(12) = valstruct(12) + get_force_difference(istru, L)*wu(L)
   end if
   
   ! 2b. General structure-based structures with a (gate) door.
   if (any(istrtypein == (/ ST_GENERAL_ST /))) then ! TODO: ST_GATE
      k1 = ln(1,L)
      k2 = ln(2,L)

      genstr => network%sts%struct(istru)%generalst
      if (hu(L) > 0) then
         valstruct(17) = valstruct(17) + get_discharge_through_gate_opening(genstr, L0, s1(k1), s1(k2))*dir
         valstruct(18) = valstruct(18) + get_discharge_over_gate(genstr, L0, s1(k1), s1(k2))*dir
         valstruct(19) = valstruct(19) + get_discharge_under_gate(genstr, L0, s1(k1), s1(k2))*dir
      
         valstruct(20) = valstruct(20) + genstr%au(3,L0) ! flow area through gate opening
         valstruct(21) = valstruct(21) + genstr%au(2,L0) ! flow area over gate
         valstruct(22) = valstruct(22) + genstr%au(1,L0) ! flow area under gate
      end if
   end if
   
   ! 2c. More specific values that apply to bridge
   if (istrtypein == ST_BRIDGE) then
      valstruct(8)  = valstruct(8) + bl(ku)*wu(L)
      valstruct(9)  = valstruct(9) + bl(kd)*wu(L)
      valstruct(10) = valstruct(10) + network%sts%struct(istru)%bridge%bedLevel_actual*wu(L)
   end if

   ! 2d. More specific value that applies to long culvert
   if (istrtypein == ST_LONGCULVERT) then
      valstruct(8) = longculverts(istru)%valve_relative_opening
   end if

end subroutine fill_valstruct_perlink


!> Averages the values on one structure across all links,
!! where needed taking care of partition models.
!! Note 1: fill_valstructs_perlink must have been called in
!! a loop prior to calling this averaging routine.
!! Note 2: if it is a general structure (jagenst == 1), then (6)-(12) are computed as well.
!! Note 3: if in parallel computing, MPI reduction must be done before calling this subroutine.
subroutine average_valstruct(valstruct, istrtypein, istru, nlinks, icount)
   use m_missing, only: dmiss
   use m_partitioninfo, only: jampi
   use m_1d_structures
   use m_General_Structure, only: t_GeneralStructure
   use m_GlobalParameters
   implicit none
   double precision, dimension(:), intent(inout) :: valstruct   !< Output values on structure (e.g. valpump(:)):
                                                                !< (1) total width (unchanged)
                                                                !< (2) structure discharge (unchanged)
                                                                !< (3) structure water level up (averaged)
                                                                !< (4) structure water level down (averaged)
                                                                !< (5) structure head (averaged)
                                                                !< (6) flow area (unchanged)
                                                                !< (7) velocity (computed)
                                                                !< (8) water level on crest (averaged)
                                                                !< (9) crest level (computed)
                                                                !< (10) crest width (computed)
                                                                !< (11) state (if all links have the same state, then write it. Otherwise it is missing value)
                                                                !< (12) force difference per unit width (averaged)
                                                                !< (13) gate opening width
                                                                !< (14) gate lower edge level
                                                                !< (15) gate opening height
                                                                !< (16) gate upper edge level
                                                                !< (17) discharge through gate opening
                                                                !< (18) discharge over gate
                                                                !< (19) discharge under gate
                                                                !< (20) flow area in gate opening
                                                                !< (21) flow area over gate
                                                                !< (22) flow area under gate
                                                                !< (23) velocity through gate opening
                                                                !< (24) velocity over gate
                                                                !< (25) velocity under gate
                                                                !< (icount) counters of partitions for parallel
   integer,                        intent(in   ) :: istrtypein  !< The type of the structure. May differ from the struct%type, for example:
                                                                !< an orifice should be called with istrtypein = ST_ORIFICE, whereas its struct(istru)%type = ST_GENERAL_ST.
   integer,                        intent(in   ) :: istru       !< Structure index in network%sts set or in longculverts
   integer,                        intent(in   ) :: nlinks      !< Number of flow links for this structure (on the current partition)
   integer,                        intent(in   ) :: icount      !< Index of the counter element in valstruct array,
                                                                !! it is the last element of the array.

   type(t_structure), pointer :: pstru
   
   ! 1. Generic values that apply to all structure types
   if (valstruct(1) == 0d0 ) then ! zero width
      valstruct(2) = dmiss  ! discharge
      valstruct(3) = dmiss  ! s1up
      valstruct(4) = dmiss  ! s1down
      valstruct(5) = dmiss  ! head
      if (istrtypein /= ST_PUMP) then
         valstruct(6) = dmiss ! flow area
         valstruct(7) = dmiss ! velocity
      end if

      if (any(istrtypein == (/ ST_GENERAL_ST, ST_WEIR, ST_ORIFICE /))) then ! TODO: ST_GATE
         valstruct(8) = dmiss ! water level on crest
         valstruct(9) = dmiss ! crest level
         valstruct(10)= dmiss ! crest width
         valstruct(11)= dmiss ! state
         valstruct(12)= dmiss ! force difference per unit width
      end if
   else
      ! valstruct(2): keep discharge at the summed value
      ! Average the remaining values:
      valstruct(3) = valstruct(3) / valstruct(1)        ! s1up
      valstruct(4) = valstruct(4) / valstruct(1)        ! s1down
      valstruct(5) = valstruct(5) / valstruct(1)        ! head

      if (istrtypein /= ST_PUMP) then
         if (valstruct(6) > 0d0) then ! non-zero flow area
            valstruct(7) = valstruct(2) / valstruct(6)  ! velocity
         else
            valstruct(7) = 0d0
         end if
      end if

      if (any(istrtypein == (/ ST_GENERAL_ST, ST_WEIR, ST_ORIFICE /))) then ! TODO: ST_GATE
         pstru => network%sts%struct(istru)
         valstruct(8) = valstruct(8) / valstruct(1)     ! water level on crest
         valstruct(12)= valstruct(12)/ valstruct(1)     ! force difference per unit width
      end if
   endif

   ! 2. More specific valus that apply to certain structure types only
   ! General structure-based structures with a (gate) door.
   if (any(istrtypein == (/ ST_GENERAL_ST, ST_ORIFICE /))) then ! TODO: ST_GATE
      if (valstruct(1) == 0d0) then ! zero width
         valstruct(13:) = dmiss
      else
         ! only for general structure
         if (istrtypein == ST_GENERAL_ST) then 
            if (valstruct(20) > 0) then ! flow area in gate opening
               valstruct(23) = valstruct(17) / valstruct(20) ! velocity through gate opening
            end if
            if (valstruct(21) > 0) then ! flow area over gate
               valstruct(24) = valstruct(18) / valstruct(21) ! velocity over gate
            end if
            if (valstruct(22) > 0) then ! flow area under gate
               valstruct(25) = valstruct(19) / valstruct(22) ! velocity under gate
            end if
         end if
      end if
   end if
   
   ! 3. More specific values that apply to bridge
   if (istrtypein == ST_BRIDGE) then
      if (valstruct(1) == 0d0 ) then ! zero width
         valstruct(8) = dmiss
         valstruct(9) = dmiss
         valstruct(10)= dmiss
      else
         valstruct(8)  = valstruct(8) / valstruct(1)
         valstruct(9)  = valstruct(9) / valstruct(1)
         valstruct(10) = valstruct(10)/ valstruct(1)
      end if
   end if

end subroutine average_valstruct


!!> Gets force difference per unit width over structure (weir, gate, general structure) per link.
double precision function get_force_difference(istru, L)
   use m_missing
   use m_flowgeom, only: ln
   use m_flow, only: s1
   use m_1d_structures, only: get_crest_level
   use m_GlobalParameters
   implicit none   
   integer, intent(in   )   :: istru !< structure index
   integer, intent(in   )   :: L     !< current link L
   
   double precision  :: s1up   !< water level up
   double precision  :: s1dn   !< water level down
   double precision  :: crestl
   integer           :: k1, k2
   double precision  :: rholeft, rhoright
   
   crestl = get_crest_level(network%sts%struct(istru))
  
   k1 = ln(1,L)
   k2 = ln(2,L)
   s1up = max(s1(k1), s1(k2))
   s1dn = min(s1(k1), s1(k2))
   if (crestl > dmiss + 0.1d0) then
      rholeft  = 1000.0d0
      rhoright = 1000.0d0
      
      get_force_difference =  max((s1up - crestl), 0.0d0)**2 * rholeft  * gravity / 2.0d0 -  &
                            max((s1dn - crestl), 0.0d0)**2 * rhoright * gravity / 2.0d0
   else
      get_force_difference = dmiss
   end if

end function get_force_difference


!> Gets discharge through gate opening per link.
double precision function get_discharge_through_gate_opening(genstr, L0, s1m1, s1m2)
   use m_missing
   use m_General_Structure
   implicit none   
   type(t_GeneralStructure), pointer, intent(in   ) :: genstr !< Derived type containing general structure information.
   integer,                           intent(in   ) :: L0     !< Local link index in genstr%..(:) link-based arrays.
   double precision,                  intent(in   ) :: s1m1   !< (geometrical) upstream water level.
   double precision,                  intent(in   ) :: s1m2   !< (geometrical) downstream water level.
   double precision  :: u1L, dsL, gatefraction
   
   dsL = s1m2 - s1m1 
   gatefraction = genstr%gateclosedfractiononlink(L0)
   
   if (gatefraction > gatefrac_eps) then
      u1L = genstr%ru(3,L0) - genstr%fu(3,L0)*dsL
      get_discharge_through_gate_opening = genstr%au(3,L0) * u1L
   else
      get_discharge_through_gate_opening = 0d0
   end if

end function get_discharge_through_gate_opening

!> Gets discharge over gate opening per link.
double precision function get_discharge_over_gate(genstr, L0, s1m1, s1m2)
   use m_missing
   use m_General_Structure
   implicit none   
   type(t_GeneralStructure), pointer, intent(in   ) :: genstr !< Derived type containing general structure information
   integer,                           intent(in   ) :: L0     !< Local link index in genstr%..(:) link-based arrays.
   double precision,                  intent(in   ) :: s1m1   !< (geometrical) upstream water level.
   double precision,                  intent(in   ) :: s1m2   !< (geometrical) downstream water level.
   double precision  :: u1L, dsL, gatefraction
   
   dsL = s1m2 - s1m1
   gatefraction = genstr%gateclosedfractiononlink(L0)
   
   if (gatefraction > gatefrac_eps) then
      u1L = genstr%ru(2,L0) - genstr%fu(2,L0)*dsL
      get_discharge_over_gate = genstr%au(2,L0) * u1L
   else
      get_discharge_over_gate = 0d0
   end if

end function get_discharge_over_gate

!> Gets discharge under gate per link.
double precision function get_discharge_under_gate(genstr, L0, s1m1, s1m2)
   use m_missing
   use m_General_Structure
   implicit none   
   type(t_GeneralStructure), pointer, intent(in   ) :: genstr !< Derived type containing general structure information
   integer,                           intent(in   ) :: L0     !< Local link index in genstr%..(:) link-based arrays.
   double precision,                  intent(in   ) :: s1m1   !< (geometrical) upstream water level.
   double precision,                  intent(in   ) :: s1m2   !< (geometrical) downstream water level.
   double precision  :: u1L, dsL, gatefraction
   
   dsL = s1m2 - s1m1
   gatefraction = genstr%gateclosedfractiononlink(L0)
   
   if (gatefraction > gatefrac_eps) then
      u1L = genstr%ru(1,L0) - genstr%fu(1,L0)*dsL
      get_discharge_under_gate = genstr%au(1,L0) * u1L
   else
      get_discharge_under_gate = 0d0
   end if

end function get_discharge_under_gate

!> Updates structure parameters for the output to restart file.
!! Only computes the needed values, and
!! only when they are not computed for history output.
!! Values are stored in the val*(:,:) arrays, shared with history output.
subroutine structure_parameters_rst()
   use m_1d_structures
   use m_flowexternalforcings
   implicit none
   integer :: n, istru
   type(t_structure), pointer    :: pstru

   do n = 1, network%sts%numCulverts
      istru = network%sts%culvertIndices(n)
      pstru => network%sts%struct(istru)
      valculvert(11,n) = get_opening_height(pstru)
   end do

   do n = 1, network%sts%numGeneralStructures
      istru = network%sts%generalStructureIndices(n)
      pstru => network%sts%struct(istru)
      valgenstru(9,n)  = get_crest_level(pstru)
      valgenstru(10,n) = get_width(pstru)
      valgenstru(14,n) = get_gle(pstru)
      valgenstru(13,n) = network%sts%struct(istru)%generalst%gateopeningwidth_actual
      ! fu, ru, au have been computed in each computational time step, so skip computing them again
   end do
   
   do n = 1, network%sts%numWeirs
      istru = network%sts%weirIndices(n)
      pstru => network%sts%struct(istru)
      valweirgen(9,n)  = get_crest_level(pstru)
      valweirgen(10,n) = get_width(pstru)
      ! fu, ru have been computed in each computational time step, so skip computing them again
   end do
   
   do n = 1, network%sts%numOrifices
      istru = network%sts%orificeIndices(n)
      pstru => network%sts%struct(istru)
      valorifgen(9,n)  = get_crest_level(pstru)
      valorifgen(10,n) = get_width(pstru)
      valorifgen(14,n) = get_gle(pstru)
      valorifgen(13,n) = network%sts%struct(istru)%generalst%gateopeningwidth_actual
      ! fu, ru have been computed in each computational time step, so skip computing them again
   end do

   do n = 1, network%sts%numPumps
      istru = network%sts%pumpIndices(n)
      pstru => network%sts%struct(istru)
      valpump(6,n) = GetPumpCapacity(pstru)
   end do

end subroutine structure_parameters_rst

!> Get the maximal number of links of all general structures/weir/orifice, when given the type and the total number of the structure
integer function get_max_numLinks(istrtypein, nstru)
   use m_1d_structures
   use m_GlobalParameters

   implicit none
   integer, intent(in   ) :: istrtypein  !< The type of the structure. May differ from the struct%type, for example:
                                         !< an orifice should be called with istrtypein = ST_ORIFICE, whereas its struct(istru)%type = ST_GENERAL_ST.
   integer, intent(in   ) :: nstru       !< Total number of this structure

   integer :: i
   
   get_max_numLinks = 0
   do i = 1, nstru
      select case (istrtypein)
      case (ST_WEIR)
         get_max_numLinks = max(get_max_numLinks, network%sts%struct(network%sts%weirIndices(i))%numlinks)
      case (ST_ORIFICE)
         get_max_numLinks = max(get_max_numLinks, network%sts%struct(network%sts%orificeIndices(i))%numlinks)
      case (ST_GENERAL_ST)
         get_max_numLinks = max(get_max_numLinks, network%sts%struct(network%sts%generalStructureIndices(i))%numlinks)
      end select
   end do

end function get_max_numLinks

!!> Gets istru when given a structure type and structure index
integer function get_istru(istrtypein, i)
   use m_1d_structures
   use m_GlobalParameters
   implicit none
   integer, intent(in   ) :: istrtypein  !< The type of the structure. May differ from the struct%type, for example:
                                         !< an orifice should be called with istrtypein = ST_ORIFICE, whereas its struct(istru)%type = ST_GENERAL_ST.
   integer, intent(in   ) :: i           !< Structure index

   select case (istrtypein)
   case (ST_WEIR)
      get_istru = network%sts%weirIndices(i)
   case (ST_ORIFICE)
      get_istru = network%sts%orificeIndices(i)
   case (ST_GENERAL_ST)
      get_istru = network%sts%generalStructureIndices(i)
   case (ST_CULVERT)
      get_istru = network%sts%culvertIndices(i)
   case (ST_UNI_WEIR)
      get_istru = network%sts%uniweirIndices(i)
   case (ST_BRIDGE)
      get_istru = network%sts%bridgeIndices(i)
   case (ST_PUMP)
      get_istru = network%sts%pumpIndices(i)
   end select
end function get_istru

!> Gets number of geometry nodes for a single structure type and structure index.
!! Geometry nodes can be used in a (multi-) polyline representation of the placement
!! of a structure on flow links.
integer function get_number_of_geom_nodes(istrtypein, i)
   use m_1d_structures
   use m_longculverts
   use m_GlobalParameters, only: ST_LONGCULVERT
   use m_partitioninfo, only: my_rank, jampi, idomain, link_ghostdata
   use m_flowgeom, only: ln
   implicit none
   integer, intent(in   ) :: istrtypein  !< The type of the structure. May differ from the struct%type, for example:
                                         !< an orifice should be called with istrtypein = ST_ORIFICE, whereas its struct(istru)%type = ST_GENERAL_ST.
   integer, intent(in   ) :: i           !< Structure index for this structure type.

   integer :: istru, nLinks, nLinksTmp, jaghost, idmn_ghost, L, Lf, La
   type(t_structure), pointer    :: pstru

   if (istrtypein == ST_LONGCULVERT) then
      get_number_of_geom_nodes = longculverts(i)%numlinks+1
   else
      istru = get_istru(istrtypein, i)

      pstru => network%sts%struct(istru)
      nLinks = pstru%numlinks
      if (jampi > 0) then ! For parallel computing, check if there is any ghost links
         nLinksTmp = nLinks
         do L = 1, nLinksTmp
            Lf = pstru%linknumbers(L)
            La = abs(Lf)
            call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
            if ( jaghost.eq.1 ) then
               nLinks = nLinks - 1
            end if
         enddo
      end if
      if (nLinks > 0) then
         ! "2D" representation: nLinks+1 polyline points.
         ! TODO: for multiple 1D links in a single structure, we could consider
         !       a multi-part polyline. That would mean: get_number_of_geom_nodes = 2*nLinks
         get_number_of_geom_nodes = nLinks + 1
      else if (nLinks == 0) then
         ! When no links: empty geometry.
         get_number_of_geom_nodes = 0
      end if
   end if

end function get_number_of_geom_nodes

!> Gets total number of geometry nodes for a given structure type and total number of the structures.
!! Geometry nodes can be used in a (multi-) polyline representation of the placement
!! of structures on flow links.
integer function get_total_number_of_geom_nodes(istrtypein, nstru)
   use m_1d_structures
   implicit none
   integer, intent(in   ) :: istrtypein  !< The type of the structure. May differ from the struct%type, for example:
                                         !< an orifice should be called with istrtypein = ST_ORIFICE, whereas its struct(istru)%type = ST_GENERAL_ST.
   integer, intent(in   ) :: nstru       !< Total number of structures of this structure type

   integer :: i, istru, nNodes, nLinks
   type(t_structure), pointer    :: pstru

   get_total_number_of_geom_nodes = 0
   do i = 1, nstru
      nNodes = get_number_of_geom_nodes(istrtypein, i)
      get_total_number_of_geom_nodes = get_total_number_of_geom_nodes + nNodes
   end do

end function get_total_number_of_geom_nodes

!> Gets geometry coordinates of a structure.
!! Geometry coordinates can be used in a (multi-) polyline representation of the placement
!! of structures on flow links.
subroutine get_geom_coordinates_of_structure(istrtypein, i, nNodes, x, y)
   use m_1d_structures
   use m_alloc
   use m_flowgeom, only: lncn
   use network_data, only: xk, yk
   use m_longculverts
   use m_GlobalParameters, only: ST_LONGCULVERT
   use m_partitioninfo, only: jampi, idomain, my_rank, link_ghostdata
   use m_flowgeom, only: ln
   implicit none
   integer,                       intent(in   ) :: istrtypein  !< The type of the structure. May differ from the struct%type, for example:
                                                               !< an orifice should be called with istrtypein = ST_ORIFICE, whereas its struct(istru)%type = ST_GENERAL_ST.
   integer,                       intent(in   ) :: i           !< Structure index for this structure type.
   integer,                       intent(in   ) :: nNodes      !< Number of geometry nodes in this structure (as computed by get_number_of_geom_nodes()).
   double precision, allocatable, intent(  out) :: x(:)        !< x-coordinates of the structure (will be reallocated when needed)
   double precision, allocatable, intent(  out) :: y(:)        !< y-coordinates of the structure (will be reallocated when needed)

   integer :: istru, nLinks, L, L0, k1, k2, k3, k4, k, nLinksTmp, jaghost, idmn_ghost, Lf, La
   double precision :: dtmp
   type(t_structure), pointer    :: pstru
   integer, allocatable :: links(:)

   if (istrtypein == ST_LONGCULVERT) then
      nLinks = longculverts(i)%numlinks
   else
      istru = get_istru(istrtypein, i)
      pstru => network%sts%struct(istru)
      nLinks = pstru%numlinks
      if (jampi > 0) then ! In parallel, if finds a ghost link, then remove it from the list
         call realloc(links, nLinks, KeepExisting=.false., fill = 0)
         nLinksTmp = nLinks
         nLinks    = 0
         do L = 1, nLinksTmp
            Lf = pstru%linknumbers(L)
            La = abs(Lf)
            call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
            if ( jaghost == 0 ) then
               nLinks = nLinks + 1
               links(nLinks) = Lf
            end if
         enddo
      end if
   end if


   if (nNodes > 0) then
      call realloc(x, nNodes, keepExisting = .false.)
      call realloc(y, nNodes, keepExisting = .false.)

      if (istrtypein == ST_LONGCULVERT) then
         L = longculverts(i)%flowlinks(1)
      else
         if (jampi > 0) then
            L = abs(links(1))
         else
            L = abs(pstru%linknumbers(1))
         end if
      end if

      k1 = lncn(1,L)
      k2 = lncn(2,L)

      x(1) = xk(k1)
      x(2) = xk(k2)
      y(1) = yk(k1)
      y(2) = yk(k2)
      k = 3
      do L0 = 2, nLinks
         if (istrtypein == ST_LONGCULVERT) then
            L = longculverts(i)%flowlinks(L0)
         else
            if (jampi > 0) then
               L = abs(links(L0))
            else
               L = abs(pstru%linknumbers(L0))
            end if
         end if
         k3 = lncn(1,L)
         k4 = lncn(2,L)
         if (L0 == 2) then
            if (k1 == k3 .or. k1 == k4) then
               dtmp = x(2)
               x(2) = x(1)
               x(1) = dtmp
               dtmp = y(2)
               y(2) = y(1)
               y(1) = dtmp
            endif
         endif
         if (k1 == k3) then
            x(k) = xk(k4)
            y(k) = yk(k4)
         else if (k1 == k4) then
            x(k) = xk(k3)
            y(k) = yk(k3)
         else if (k2 == k3) then
            x(k) = xk(k4)
            y(k) = yk(k4)
         else if (k2 == k4) then
            x(k) = xk(k3)
            y(k) = yk(k3)
         endif
         k1 = k3
         k2 = k4
         k = k+1
      end do
   end if
end subroutine get_geom_coordinates_of_structure

!> Gets geometry coordinates of a structure, aligned along structure.
!! Geometry coordinates can be used in a polyline representation of the placement
!! of structures on flow links.
subroutine get_geom_coordinates_of_structure_old(i, nNodes, x, y)
   use m_alloc
   use m_flowexternalforcings, only: ncgensg, kcgen, L1cgensg, L2cgensg
   use m_flowgeom, only: lncn
   use network_data, only: xk, yk
   implicit none
   integer,                       intent(in   ) :: i           !< Structure index for this structure type.
   integer,                       intent(in   ) :: nNodes      !< Number of geometry nodes in this structure.
   double precision, allocatable, intent(  out) :: x(:)   !< x-coordinates of the structure (will be reallocated when needed)
   double precision, allocatable, intent(  out) :: y(:)   !< y-coordinates of the structure (will be reallocated when needed)

   integer :: L, L0, k1, k2, k3, k4, k
   double precision :: dtmp

   if (nNodes > 0) then
      call realloc(x, nNodes)
      call realloc(y, nNodes)

      L0 = L1cgensg(i)
      L = abs(kcgen(3,L0))
      k1 = lncn(1,L)
      k2 = lncn(2,L)
      x(1) = xk(k1)
      x(2) = xk(k2)
      y(1) = yk(k1)
      y(2) = yk(k2)
                      
      k = 3
      do L0 = L1cgensg(i)+1, L2cgensg(i)
         L = abs(kcgen(3,L0))
         k3 = lncn(1,L)
         k4 = lncn(2,L)
         if (L0 == 2) then
            if (k1 == k3 .or. k1 == k4) then
               dtmp = x(2)
               x(2) = x(1)
               x(1) = dtmp
               dtmp = y(2)
               y(2) = y(1)
               y(1) = dtmp
            endif
         endif
         if (k1 == k3) then
            x(k) = xk(k4)
            y(k) = yk(k4)
         else if (k1 == k4) then
            x(k) = xk(k3)
            y(k) = yk(k3)
         else if (k2 == k3) then
            x(k) = xk(k4)
            y(k) = yk(k4)
         else if (k2 == k4) then
            x(k) = xk(k3)
            y(k) = yk(k3)
         endif
         k1 = k3
         k2 = k4
         k = k+1
      end do
   end if
end subroutine get_geom_coordinates_of_structure_old

!> Fills in the geometry arrays of a structure type for history output
subroutine fill_geometry_arrays_structure(istrtypein, nstru, nNodesStru, nodeCountStru, geomXStru, geomYStru)
   use m_alloc
   use m_partitioninfo
   use m_GlobalParameters
   implicit none
   integer,                       intent(in   ) :: istrtypein       !< The type of the structure. May differ from the struct%type
   integer,                       intent(in   ) :: nstru            !< Number of this structure type
   integer,                       intent(  out) :: nNodesStru       !< Total number of nodes of this structure type
   integer,          allocatable, intent(  out) :: nodeCountStru(:) !< Node count of this structure type
   double precision, allocatable, intent(  out) :: geomXStru(:)     !< [m] x coordinate of nodes of this structure type
   double precision, allocatable, intent(  out) :: geomYStru(:)     !< [m] y coordinate of nodes of this structure type

   double precision, allocatable :: xGat(:), yGat(:)     ! Coordinates that are gatherd data from all subdomains
   integer,          allocatable :: nodeCountStruMPI(:)  ! Count of nodes per structure after mpi communication.
   double precision, allocatable :: geomXStruMPI(:)      ! [m] x coordinates of structures after mpi communication.
   double precision, allocatable :: geomYStruMPI(:)      ! [m] y coordinates of structures after mpi communication.
   integer,          allocatable :: nodeCountStruGat(:), nNodesStruGat(:), displs(:)
   double precision, allocatable :: geomX(:), geomY(:)
   integer                       :: i, j, k, k1, ierror, is, ie, n, ii, nNodes, nNodesStruMPI

   ! Allocate and construct geometry variable arrays (on one subdomain)
   call realloc(nodeCountStru,   nstru, keepExisting = .false., fill = 0  )
   do i = 1, nstru
      nNodes = get_number_of_geom_nodes(istrtypein, i)
      nodeCountStru(i) = nNodes
   end do
   nNodesStru = sum(nodeCountStru)
   call realloc(geomXStru,       nNodesStru,   keepExisting = .false., fill = 0d0)
   call realloc(geomYStru,       nNodesStru,   keepExisting = .false., fill = 0d0)
   is = 0
   ie = 0
   do i = 1, nstru
      nNodes = nodeCountStru(i)
      if (nNodes > 0) then
         call get_geom_coordinates_of_structure(istrtypein, i, nNodes, geomX, geomY)
         is = ie + 1
         ie = is + nNodes - 1
         geomXStru(is:ie) = geomX(1:nNodes)
         geomYStru(is:ie) = geomY(1:nNodes)
      end if
   end do

   !! The codes below are similar to subroutine "fill_geometry_arrays_lateral".
   !! They work for 1D structures, but are supposed to work when more links are contained in a structure.
   ! For parallel simulation: since only process 0000 writes the history output, the related arrays
   ! are only made on 0000.
   if (jampi > 0) then
      call reduce_int_sum(nNodesStru, nNodesStruMPI) ! Get total number of nodes among all subdomains

      if (my_rank == 0) then
         ! Allocate arrays
         call realloc(nodeCountStruMPI, nstru,  keepExisting = .false., fill = 0  )
         call realloc(geomXStruMPI,     nNodesStruMPI, keepExisting = .false., fill = 0d0)
         call realloc(geomYStruMPI,     nNodesStruMPI, keepExisting = .false., fill = 0d0)

         ! Allocate arrays that gather information from all subdomains
         ! Data on all subdomains will be gathered in a contiguous way
         call realloc(nodeCountStruGat, nstru*ndomains, keepExisting = .false., fill = 0  )
         call realloc(xGat,             nNodesStruMPI,  keepExisting = .false., fill = 0d0)
         call realloc(yGat,             nNodesStruMPI,  keepExisting = .false., fill = 0d0)
         call realloc(displs,           ndomains,       keepExisting = .false., fill = 0  )
         call realloc(nNodesStruGat,    ndomains,       keepExisting = .false., fill = 0  )
      end if

      ! Gather integer data, where the same number of data, i.e. nstru, are gathered from each subdomain to process 0000
      call gather_int_data_mpi_same(nstru, nodeCountStru, nstru*ndomains, nodeCountStruGat, nstru, 0, ierror)

      if (my_rank == 0) then
         ! To use mpi gather call, construct displs, and nNodesStruGat (used as receive count for mpi gather call)
         displs(1) = 0
         do i = 1, ndomains
            is = (i-1)*nstru+1 ! Starting index in nodeCountStruGat
            ie = is+nstru-1    ! Endding index in nodeCountStruGat
            nNodesStruGat(i) = sum(nodeCountStruGat(is:ie)) ! Total number of nodes on subdomain i
            if (i > 1) then
               displs(i) = displs(i-1) + nNodesStruGat(i-1)
            end if
         end do
      end if

      ! Gather double precision data, here, different number of data can be gatherd from different subdomains to process 0000
      call gatherv_double_data_mpi_dif(nNodesStru, geomXStru, nNodesStruMPI, xGat, ndomains, nNodesStruGat, displs, 0, ierror)
      call gatherv_double_data_mpi_dif(nNodesStru, geomYStru, nNodesStruMPI, yGat, ndomains, nNodesStruGat, displs, 0, ierror)

      if (my_rank == 0) then
         ! Construct nodeCountStruMPI for history output
         do i = 1, nstru
            do n = 1, ndomains
               k = (n-1)*nstru+i
               nodeCountStruMPI(i) = nodeCountStruMPI(i) + nodeCountStruGat(k) ! Total number of nodes for structure i among all subdomains
            end do
         end do

         ! Construct geomXStruMPI and geomYStruMPI for history output
         j = 1
         do i = 1, nstru    ! for each structure
            do n = 1, ndomains ! on each sudomain
               k = (n-1)*nstru+i        ! index in nodeCountStruGat
               nNodes = nodeCountStruGat(k)  ! structure i on sumdomain n has nNodes nodes
               if (nNodes > 0) then
                  ii = (n-1)*nstru
                  is = sum(nNodesStruGat(1:n-1)) + sum(nodeCountStruGat(ii+1:ii+i-1))! starting index in xGat
                  do k1 = 1, nNodes
                     geomXStruMPI(j) = xGat(is+k1)
                     geomYStruMPI(j) = yGat(is+k1)
                     j = j + 1
                  end do
               end if
            end do
         end do
         ! Copy the MPI-arrays to nodeCoutLat, geomXStru and geomYStru for the his-output
         nNodesStru = nNodesStruMPI
         nodeCountStru(1:nstru) = nodeCountStruMPI(1:nstru)
         call realloc(geomXStru, nNodesStru, keepExisting = .false., fill = 0d0)
         call realloc(geomYStru, nNodesStru, keepExisting = .false., fill = 0d0)
         geomXStru(1:nNodesStru) = geomXStruMPI(1:nNodesStru)
         geomYStru(1:nNodesStru) = geomYStruMPI(1:nNodesStru)
      end if
   end if
end subroutine fill_geometry_arrays_structure

!> Fill in array valstruct for a givin general structure, weir or orifice.
subroutine fill_valstruct_per_structure(valstruct, istrtypein, istru, nlinks)
   use m_missing, only: dmiss
   use m_1d_structures
   use m_General_Structure, only: t_GeneralStructure
   use m_GlobalParameters
   implicit none
   double precision, dimension(:), intent(inout) :: valstruct     !< Output values on structure (e.g. valweirgen(:)):
                                                                  !< (1) total width
                                                                  !< (2) structure discharge
                                                                  !< (3) structure water level up
                                                                  !< (4) structure water level down
                                                                  !< (5) structure head
                                                                  !< (6) flow area
                                                                  !< (7) velocity
                                                                  !< (8) water level on crest, or valve relative opening if type is long culvert
                                                                  !< (9) crest level
                                                                  !< (10) crest width
                                                                  !< (11) state
                                                                  !< (12) force difference per unit width
                                                                  !< (13) gate opening width
                                                                  !< (14) gate lower edge level
                                                                  !< (15) gate opening height
                                                                  !< (16) gate upper edge level
                                                                  !< (17) discharge through gate opening
                                                                  !< (18) discharge over gate
                                                                  !< (19) discharge under gate
                                                                  !< (20) flow area in gate opening
                                                                  !< (21) flow area over gate
                                                                  !< (22) flow area under gate
                                                                  !< (23) velocity through gate opening
                                                                  !< (24) velocity over gate
                                                                  !< (25) velocity under gate
   integer,                           intent(in   ) :: istrtypein !< Structure type
   integer,                           intent(in   ) :: istru      !< Structure index in network%sts set or in longculverts
   integer,                           intent(in   ) :: nlinks     !< Number of links for the structure
   
   double precision :: tmp
   integer          :: jadif,i
   type(t_structure), pointer :: pstru
   type(t_GeneralStructure), pointer :: genstr

   if (any(istrtypein == (/ ST_GENERAL_ST, ST_WEIR, ST_ORIFICE /))) then ! TODO: ST_GATE
      pstru => network%sts%struct(istru)
      valstruct(9) = get_crest_level(pstru)     ! crest level
      valstruct(10)= get_width(pstru)           ! crest width
      
      ! determine state
      tmp = maxval(pstru%generalst%state(1:3,1))
      jadif = 0
      do i = 2, nlinks
         if (tmp /= maxval(pstru%generalst%state(1:3,i))) then
            jadif = 1
            exit
         end if
      end do
      if (jadif == 0) then
         valstruct(11) = dble(tmp)
      else
         valstruct(11) = dmiss
      end if
   end if

   if (any(istrtypein == (/ ST_GENERAL_ST, ST_ORIFICE /))) then ! TODO: ST_GATE
      if (nlinks > 0) then ! If it is a new general structure, and there are links
         genstr => network%sts%struct(istru)%generalst
         valstruct(13) = genstr%gateopeningwidth_actual           ! gate opening width
         valstruct(14) = get_gle(pstru)                           ! gate lower edge level
         valstruct(15) = get_opening_height(pstru)                ! gate opening height
         valstruct(16) = valstruct(14) + genstr%gatedoorheight    ! gate upper edge level
      end if
   end if

end subroutine fill_valstruct_per_structure
end module m_structures