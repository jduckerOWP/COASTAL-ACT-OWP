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

! $Id: structure_parameters.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/structure_parameters.f90 $

! =================================================================================================
! =================================================================================================
   subroutine structure_parameters
      use m_flowgeom , only : ln, wu, bob
      use m_flow
      use m_structures
      use m_flowexternalforcings, only: ngenstru
      use m_partitioninfo
      use m_flowtimes
      use m_missing, only: dmiss
      use unstruc_channel_flow, only: network
      use m_1d_structures
      use m_compound
      use m_GlobalParameters
      use m_longculverts, only: nlongculvertsg, longculverts
      implicit none
      integer                       :: i, n, L, Lf, La, ierr, ntmp, k, ku, kd, istru, nlinks
      double precision              :: dir
      integer                       :: jaghost, idmn_ghost, jaghostexist
      double precision, save        :: timprev = -1d0
      double precision              :: timstep
      type(t_structure), pointer    :: pstru
      type(t_compound),  pointer    :: pcmp

      if (jampi > 0) then
         if( .not.allocated( reducebuf ) ) then
            nreducebuf = npumpsg*NUMVALS_PUMP + ngatesg*NUMVALS_GATE + ncdamsg*NUMVALS_CDAM + ncgensg*NUMVALS_CGEN &
                       + ngategen*NUMVALS_GATEGEN + nweirgen*NUMVALS_WEIRGEN + ngenstru*NUMVALS_GENSTRU + ngenstru*NUMVALS_GENSTRU &
                       + ndambreaksg * NUMVALS_DAMBREAK + network%sts%numUniWeirs*NUMVALS_UNIWEIR + network%sts%numOrifices*NUMVALS_ORIFGEN &
                       + network%sts%numCulverts*NUMVALS_CULVERT + network%sts%numBridges*NUMVALS_BRIDGE + network%cmps%count*NUMVALS_CMPSTRU
            allocate ( reducebuf  ( nreducebuf ) , stat = ierr      )
            call aerr('reducebuf  ( nreducebuf )', ierr, nreducebuf ) ; reducebuf  = 0d0
         endif
      endif

      if (ti_his <= 0) return
      ! in order to compute the cumulative discharge, we have to compute the time step (see update updateValuesOnCrossSections)
      if (timprev == -1d0) then
        timstep  = 0d0
      else
        timstep  = time1 - timprev
      end if
      !
      ! === Pumps
      !
      if (allocated(valpump)) then
         do n = 1,npumpsg
            valpump(1:5,n) = 0d0
            valpump(6:NUMVALS_PUMP,n) = dmiss
            if (allocated(pumpsWithLevels)) then
               istru = pumpsWithLevels(n)
            else
               istru = -1
            end if

            do L = L1pumpsg(n),L2pumpsg(n)
               Lf = kpump(3,L)
               La = abs( Lf )
               if( jampi > 0 ) then
                  call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
                  if ( jaghost.eq.1 ) cycle
               endif
               dir = 1d0
               if( Ln(1,La) /= kpump(1,L) ) then
                  dir = -1d0
               end if
               call fill_valstruct_perlink(valpump(:,n), La, dir, ST_PUMP, istru, L-L1pumpsg(n)+1)
            enddo
         enddo
      end if
      !
      ! === Gates
      !
      if (allocated(valgate)) then
         do n = 1,ngatesg
            valgate(:,n) = 0d0
            do L = L1gatesg(n), L2gatesg(n)
               Lf = kgate(3,L)
               La = abs( Lf )
               if( jampi > 0 ) then
                  call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
                  if ( jaghost.eq.1 ) cycle
               endif
               dir = 1d0
               ku = ln(1,La)
               kd = ln(2,La)
               if( Ln(1,La) /= kgate(1,L) ) then
                  dir = -1d0
                  ku = ln(2,La)
                  kd = ln(1,La)
               end if
               valgate(1,n) = valgate(1,n) + wu(La)
               valgate(2,n) = valgate(2,n) + q1(La) * dir
               valgate(3,n) = valgate(3,n) + s1(ku) * wu(La)
               valgate(4,n) = valgate(4,n) + s1(kd) * wu(La)
            enddo
            if( jampi == 0 ) then
               if( valgate(1,n) == 0d0 ) then
                  valgate(2,n) = dmiss
                  valgate(3,n) = dmiss
                  valgate(4,n) = dmiss
               else
                  valgate(3,n) = valgate(3,n) / valgate(1,n)
                  valgate(4,n) = valgate(4,n) / valgate(1,n)
               endif
            endif
         enddo
      end if
      !
      ! === Dams
      !
      if (allocated(valcdam)) then
         do n = 1,ncdamsg
            valcdam(:,n) = 0d0
            do L = L1cdamsg(n), L2cdamsg(n)
               Lf = kcdam(3,L)
               La = abs( Lf )
               if( jampi > 0 ) then
                  call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
                  if ( jaghost.eq.1 ) cycle
               endif
               dir = 1d0
               ku = ln(1,La)
               kd = ln(2,La)
               if( Ln(1,La) /= kcdam(1,L) ) then
                  dir = -1d0
                  ku = ln(2,La)
                  kd = ln(1,La)
               end if
               valcdam(1,n) = valcdam(1,n) + wu(La)
               valcdam(2,n) = valcdam(2,n) + q1(La) * dir
               valcdam(3,n) = valcdam(3,n) + s1(ku) * wu(La)
               valcdam(4,n) = valcdam(4,n) + s1(kd) * wu(La)
            enddo
            if( jampi == 0 ) then
               if( valcdam(1,n) == 0d0 ) then
                  valcdam(2,n) = dmiss
                  valcdam(3,n) = dmiss
                  valcdam(4,n) = dmiss
               else
                  valcdam(3,n) = valcdam(3,n) / valcdam(1,n)
                  valcdam(4,n) = valcdam(4,n) / valcdam(1,n)
               endif
            endif
         enddo
      end if
      !
      ! === General structures (from old ext file)
      !
      if (allocated(valcgen)) then
         do n = 1,ncgensg
            i = n
            valcgen(:,n) = 0d0
            do L = L1cgensg(i),L2cgensg(i)
               Lf = kcgen(3,L)
               La = abs( Lf )
               if( jampi > 0 ) then
                  call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
                  if ( jaghost.eq.1 ) cycle
               endif
               dir = 1d0
               ku = ln(1,La)
               kd = ln(2,La)
               if( Ln(1,La) /= kcgen(1,L) ) then
                  dir = -1d0
                  ku = ln(2,La)
                  kd = ln(1,La)
               end if
               valcgen(1,n) = valcgen(1,n) + wu(La)
               valcgen(2,n) = valcgen(2,n) + q1(La) * dir
               valcgen(3,n) = valcgen(3,n) + s1(ku) * wu(La)
               valcgen(4,n) = valcgen(4,n) + s1(kd) * wu(La)
            enddo
            if( jampi == 0 ) then
               if( valcgen(1,n) == 0d0 ) then
                  valcgen(2,n) = dmiss
                  valcgen(3,n) = dmiss
                  valcgen(4,n) = dmiss
               else
                  valcgen(3,n) = valcgen(3,n) / valcgen(1,n)
                  valcgen(4,n) = valcgen(4,n) / valcgen(1,n)
               endif
            endif
         enddo
      end if
      !
      ! === Gates (new)
      !
      if (allocated(valgategen)) then
         do n = 1,ngategen
            i = gate2cgen(n)
            valgategen(:,n) = 0d0
            do L = L1cgensg(i), L2cgensg(i)
               Lf = kcgen(3,L)
               La = abs( Lf )
               if( jampi > 0 ) then
                  call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
                  if ( jaghost.eq.1 ) cycle
               endif
               dir = 1d0
               ku = ln(1,La)
               kd = ln(2,La)
               if( Ln(1,La) /= kcgen(1,L) ) then
                  dir = -1d0
                  ku = ln(2,La)
                  kd = ln(1,La)
               end if
               valgategen(1,n) = valgategen(1,n) + wu(La)
               valgategen(2,n) = valgategen(2,n) + q1(La) * dir
               valgategen(3,n) = valgategen(3,n) + s1(ku) * wu(La)
               valgategen(4,n) = valgategen(4,n) + s1(kd) * wu(La)
               k = kcgen(1,L) ; if( q1(La) < 0d0 ) k = kcgen(2,L)
               valgategen(5,n) = valgategen(5,n) + s1(k) * wu(La)
            enddo
            if (L1cgensg(i) <= L2cgensg(i)) then ! At least one flow link in this domain is affected by this structure.
               valgategen(6,n) = 1               ! rank contains the gate.
               valgategen(7,n) = zcgen(3*i  )    ! id_gategen_openw.
               valgategen(8,n) = zcgen(3*i-1)    ! id_gategen_edgel.
               valgategen(9,n) = zcgen(3*i-2)    ! id_gategen_sillh.
            end if
            if( jampi == 0 ) then
               if( valgategen(1,n) == 0d0 ) then
                  valgategen(2,n) = dmiss
                  valgategen(3,n) = dmiss
                  valgategen(4,n) = dmiss
                  valgategen(5,n) = dmiss
               else
                  valgategen(3,n) = valgategen(3,n) / valgategen(1,n)
                  valgategen(4,n) = valgategen(4,n) / valgategen(1,n)
                  valgategen(5,n) = max( min( zcgen(3*i-1)-zcgen(3*i-2), valgategen(5,n)/valgategen(1,n)-zcgen(3*i-2) ), 0.0d0)  ! flow through height is always positive
               endif
            endif
         enddo
      end if
      !
      ! === Weirs
      !
      if (allocated(valweirgen)) then
         if (network%sts%numWeirs > 0) then ! new weir
            do n = 1, nweirgen
               valweirgen(1:NUMVALS_WEIRGEN,n) = 0d0
               istru = network%sts%weirIndices(n)
               pstru => network%sts%struct(istru)
               nlinks = pstru%numlinks
               jaghost = 0
               do L = 1, nlinks
                  Lf = pstru%linknumbers(L)
                  La = abs( Lf )
                  if( jampi > 0 ) then
                     call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
                     if ( jaghost.eq.1 ) cycle
                  endif
                  dir = sign(1d0,dble(Lf))
                  call fill_valstruct_perlink(valweirgen(:,n), La, dir, ST_WEIR, istru, L)
               enddo
               if (nlinks > 0 .and. jaghost == 0) then ! This assumes that each weir has only 1 link
                  call fill_valstruct_per_structure(valweirgen(:,n), ST_WEIR, istru, nlinks)
               end if
            enddo
         else
            ! old weir, do not compute the new extra fileds
            do n = 1, nweirgen
               i = weir2cgen(n)
               valweirgen(1:NUMVALS_WEIRGEN,n) = 0d0
               do L = L1cgensg(i),L2cgensg(i)
                  Lf = kcgen(3,L)
                  La = abs( Lf )
                  if( jampi > 0 ) then
                     call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
                     if ( jaghost.eq.1 ) cycle
                  endif
                  dir = 1d0
                  if( Ln(1,La) /= kcgen(1,L) ) then
                     dir = -1d0
                  end if
                  call fill_valstruct_perlink(valweirgen(:,n), La, dir, ST_UNSET, 0, 0)
               enddo
            enddo
         end if
      end if

      !
      ! === Orifice
      !
      if (allocated(valorifgen)) then
         do n = 1, network%sts%numOrifices
            valorifgen(1:NUMVALS_ORIFGEN,n) = 0d0
            istru = network%sts%orificeIndices(n)
            pstru => network%sts%struct(istru)
            nlinks = pstru%numlinks
            jaghost = 0
            do L = 1, nlinks
               Lf = pstru%linknumbers(L)
               La = abs( Lf )
               if( jampi > 0 ) then
                  call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
                  if ( jaghost.eq.1 ) cycle
               endif
               dir = sign(1d0,dble(Lf))
               call fill_valstruct_perlink(valorifgen(:,n), La, dir, ST_ORIFICE, istru, L)
            enddo
            if (nlinks > 0 .and. jaghost == 0) then ! This assumes that each orifice has only 1 link
               call fill_valstruct_per_structure(valorifgen(:,n), ST_ORIFICE, istru, nlinks)
            end if
         enddo
      end if

      !
      ! === Bridge
      !
      if (allocated(valbridge)) then
         do n = 1, network%sts%numBridges
            valbridge(1:NUMVALS_BRIDGE,n) = 0d0
            istru = network%sts%bridgeIndices(n)
            pstru => network%sts%struct(istru)
            nlinks = pstru%numlinks
            do L = 1, nlinks ! Currently bridges have always only 1 link.
               Lf = pstru%linknumbers(L)
               La = abs( Lf )
               if( jampi > 0 ) then
                  call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
                  if ( jaghost.eq.1 ) cycle
               endif
               dir = sign(1d0,dble(Lf))
               call fill_valstruct_perlink(valbridge(:,n), La, dir, ST_BRIDGE, istru, L)
            enddo
         enddo
      end if

      !
      ! === Culvert
      !
      if (allocated(valculvert)) then
         do n = 1, network%sts%numCulverts
            valculvert(1:NUMVALS_CULVERT,n) = 0d0
            istru = network%sts%culvertIndices(n)
            pstru => network%sts%struct(istru)
            nlinks = pstru%numlinks
            jaghost = 0
            do L = 1, nlinks
               Lf = pstru%linknumbers(L)
               La = abs( Lf )
               if( jampi > 0 ) then
                  call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
                  if ( jaghost.eq.1 ) cycle
               endif
               dir = sign(1d0,dble(Lf))
               call fill_valstruct_perlink(valculvert(:,n), La, dir, ST_CULVERT, istru, L)
            enddo

            if (nLinks > 0 .and. jaghost == 0) then ! This assumes that each culvert has only 1 link
               valculvert(8,n) = get_crest_level(pstru)
               valculvert(9,n) = dble(get_culvert_state(pstru))
               valculvert(10,n) = get_gle(pstru)
               valculvert(11,n) = get_opening_height(pstru)
            end if
         enddo
      end if

      !
      ! === Universal weir
      !
      if (allocated(valuniweir)) then
         do n = 1, network%sts%numuniweirs
            valuniweir(1:NUMVALS_UNIWEIR,n) = 0d0
            istru = network%sts%uniweirIndices(n)
            pstru => network%sts%struct(istru)
            nlinks = pstru%numlinks
            jaghost = 0
            do L = 1, nlinks
               Lf = pstru%linknumbers(L)
               La = abs( Lf )
               if( jampi > 0 ) then
                  call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
                  if ( jaghost.eq.1 ) cycle
               endif
               dir = sign(1d0,dble(Lf))
               call fill_valstruct_perlink(valuniweir(:,n), La, dir, ST_UNI_WEIR, istru, L)
            enddo
            if (nLinks > 0 .and. jaghost == 0) then ! This assumes that each universal weir has only 1 link
               valuniweir(8,n) = get_crest_level(pstru)
            end if
         enddo
      end if

      !
      ! == dambreak
      !
      if (allocated(valdambreak)) then
         do n = 1, ndambreaksg
            ! valdambreak(NUMVALS_DAMBREAK,n) is the cumulative over time, we do not reset it to 0
            valdambreak(1:NUMVALS_DAMBREAK-1,n) = 0d0
            istru = dambreaks(n)
            do L = L1dambreaksg(n),L2dambreaksg(n)
               if (activeDambreakLinks(L) /= 1) then
                  cycle
               end if

               Lf = kdambreak(3,L)
               La = abs( Lf )
               if( jampi > 0 ) then
                  call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
                  if ( jaghost.eq.1 ) cycle
               endif
               dir = 1d0
               if( Ln(1,La) /= kdambreak(1,L) ) then
                  dir = -1d0
               end if
               valdambreak(1,n) = valdambreak(1,n) + dambreakLinksActualLength(L)
               valdambreak(2,n) = valdambreak(2,n) + q1(La)*dir
               valdambreak(6,n) = valdambreak(6,n) + au(La) ! flow area
               valdambreak(9,n) = valdambreak(9,n) + dambreakLinksActualLength(L)
            enddo
            if (network%sts%struct(istru)%dambreak%width > 0d0) then
               valdambreak(8,n) = network%sts%struct(istru)%dambreak%crl ! crest level
            else
               valdambreak(1:NUMVALS_DAMBREAK-1,n) = dmiss               ! No breach started yet, set FillValue
               La = abs(kdambreak(3,LStartBreach(n)))
               valdambreak(8,n) = bob(1,La)                              ! No breach started yet, use bob as 'crest'.
               valdambreak(9,n) = 0d0                                    ! No breach started yet, set crest width to 0
               cycle
            end if
            ! TODO: UNST-5102: code below needs checking: when dambreak #n not active in current partition,
            ! most values below *are* available (based on other partitions). And in the code ahead, a call to reduce_crs
            ! assumes that all values are present and will be sum-reduced in a flowlinkwidth-weighted manner.
            valdambreak(3,n)  = waterLevelsDambreakUpStream(n)
            valdambreak(4,n)  = waterLevelsDambreakDownStream(n)
            valdambreak(5,n)  = valdambreak(3,n) - valdambreak(4,n)
            valdambreak(7,n)  = normalVelocityDambreak(n)
            valdambreak(10,n) = waterLevelJumpDambreak(n)
            valdambreak(11,n) = breachWidthDerivativeDambreak(n)
            valdambreak(12,n) = valdambreak(12,n) + valdambreak(2,n) * timstep ! cumulative discharge
         enddo
      end if
      !
      ! === General structures (from new ext file)
      !
      if (allocated(valgenstru)) then
         if (network%sts%numGeneralStructures > 0) then
            do n = 1, ngenstru
               valgenstru(1:NUMVALS_GENSTRU,n) = 0d0
               istru = network%sts%generalStructureIndices(n)
               pstru => network%sts%struct(istru)
               nlinks = pstru%numlinks
               jaghost = 0
               do L = 1, nlinks
                  Lf = pstru%linknumbers(L)
                  La = abs( Lf )
                  if( jampi > 0 ) then
                     call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
                     if ( jaghost.eq.1 ) cycle
                  endif
                  dir = sign(1d0,dble(Lf))
                  call fill_valstruct_perlink(valgenstru(:,n), La, dir, ST_GENERAL_ST, istru, L)
               enddo
               if (nlinks > 0 .and. jaghost == 0) then ! This assumes that each general structure has only 1 link
                  call fill_valstruct_per_structure(valgenstru(:,n), ST_GENERAL_ST, istru, nlinks)
               end if
            enddo
         else
            ! old general structure, do not compute the new extra fileds
            do n = 1,ngenstru
               i = genstru2cgen(n)
               valgenstru(1:NUMVALS_GENSTRU,n) = 0d0
               do L = L1cgensg(i),L2cgensg(i)
                  Lf = kcgen(3,L)
                  La = abs( Lf )
                  if( jampi > 0 ) then
                     call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
                     if ( jaghost.eq.1 ) cycle
                  endif
                  dir = 1d0
                  if( Ln(1,La) /= kcgen(1,L) ) then
                     dir = -1d0
                  end if
                 call fill_valstruct_perlink(valgenstru(:,n), La, dir, ST_UNSET, 0, 0)
               enddo
               ! Fill in values for each structure if it exists on the current subdomain
               if (L1cgensg(i) <= L2cgensg(i)) then  ! At least one flow link in this domain is affected by this structure.
                  valgenstru(NUMVALS_GENSTRU,n) = 1  ! rank contains the general structure.
                  valgenstru(13,n) = zcgen(3*i  )    ! id_genstru_openw.
                  valgenstru(14,n) = zcgen(3*i-1)    ! id_genstru_edgel.
                  valgenstru(9,n)  = zcgen(3*i-2)    ! id_genstru_cresth.
               end if
            enddo
         end if
      end if

      !
      ! === compound structure
      !
      if (allocated(valcmpstru)) then
         if (network%cmps%count > 0) then
            do n = 1, network%cmps%count
               valcmpstru(1:NUMVALS_CMPSTRU,n) = 0d0
               pcmp => network%cmps%compound(n)
               nlinks = pcmp%numlinks
               do L = 1, nlinks
                  Lf = pcmp%linknumbers(L)
                  La = abs( Lf )
                  if( jampi > 0 ) then
                     call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
                     if ( jaghost.eq.1 ) cycle
                  endif
                  dir = sign(1d0,dble(Lf))
                  call fill_valstruct_perlink(valcmpstru(:,n), La, dir, ST_COMPOUND, 0, L)
               enddo
            enddo
         end if
      end if
      !
      ! === Long culvert
      !
      if (allocated(vallongculvert)) then
         do n = 1, nlongculvertsg
            vallongculvert(1:NUMVALS_LONGCULVERT,n) = 0d0
            if (longculverts(n)%numlinks > 0) then
               Lf = longculverts(n)%flowlinks(1) ! We use the 1st link as a representative flow link
               La = abs( Lf )
               if( jampi > 0 ) then ! TODO: UNST-4644
                  call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
                  if ( jaghost.eq.1 ) cycle
               endif
               dir = sign(1d0,dble(Lf))
               call fill_valstruct_perlink(vallongculvert(:,n), La, dir, ST_LONGCULVERT, n, 0)
               call average_valstruct(vallongculvert(:,n), ST_LONGCULVERT, n, 0, 0)
            end if
         enddo
      end if

      !
      ! === Reduction of structur parameters for parallel
      !
      if( jampi > 0 .and. ti_his > 0 ) then
         nreducebuf = 0
         reducebuf  = 0d0
         n = 0
         if( npumpsg > 0 .and. allocated(valpump) ) then
            call fill_reduce_buffer( valpump   , npumpsg*NUMVALS_PUMP     )
            n = 1
         endif
         if( ngatesg > 0 .and. allocated(valgate) ) then
            call fill_reduce_buffer( valgate   , ngatesg*NUMVALS_GATE     )
            n = 1
         endif
         if( ncdamsg > 0 .and. allocated(valcdam) ) then
            call fill_reduce_buffer( valcdam   , ncdamsg*NUMVALS_CDAM     )
            n = 1
         endif
         if( ncgensg > 0 .and. allocated(valcgen) ) then
            call fill_reduce_buffer( valcgen   , ncgensg*NUMVALS_CGEN     )
            n = 1
         endif
         if( ngategen > 0 .and. allocated(valgategen) ) then
            call fill_reduce_buffer( valgategen, ngategen*NUMVALS_GATEGEN )
            n = 1
         endif
         if( nweirgen > 0 .and. allocated(valweirgen) ) then
            call fill_reduce_buffer( valweirgen, nweirgen*NUMVALS_WEIRGEN )
            n = 1
         endif
         if( ngenstru > 0 .and. allocated(valgenstru) ) then
            call fill_reduce_buffer( valgenstru, ngenstru*NUMVALS_GENSTRU )
            n = 1
         endif
         if( ndambreaksg > 0 .and. allocated(valdambreak) ) then
            call fill_reduce_buffer( valdambreak, ndambreaksg*NUMVALS_DAMBREAK )
            n = 1
         endif
         if(allocated(valuniweir) .and. network%sts%numUniWeirs > 0) then
            call fill_reduce_buffer( valuniweir, network%sts%numUniWeirs*NUMVALS_UNIWEIR )
            n = 1
         endif
         if(allocated(valorifgen) .and. network%sts%numOrifices > 0) then
            call fill_reduce_buffer( valorifgen, network%sts%numOrifices*NUMVALS_ORIFGEN )
            n = 1
         endif
         if(allocated(valculvert) .and. network%sts%numCulverts > 0) then
            call fill_reduce_buffer( valculvert, network%sts%numCulverts*NUMVALS_CULVERT )
            n = 1
         endif
         if(allocated(valbridge) .and. network%sts%numBridges > 0) then
            call fill_reduce_buffer( valbridge, network%sts%numBridges*NUMVALS_BRIDGE )
            n = 1
         endif
         if(allocated(valcmpstru) .and. network%cmps%count > 0) then
            call fill_reduce_buffer( valcmpstru, network%cmps%count*NUMVALS_CMPSTRU )
            n = 1
         endif
         ! TODO: UNST-4644, add for long culvert
         if( n == 1 ) then
            call reduce_crs(reducebuf,nreducebuf,1) 
         endif
      end if

         ! TODO: UNST-4644, add for long culvert
      ! === Compound structure
      if (network%cmps%count > 0 .and. allocated(valcmpstru)) then
         if (jampi > 0) then
            call subsitute_reduce_buffer( valcmpstru, network%cmps%count*NUMVALS_CMPSTRU )
         end if
         do n=1,network%cmps%count
            pcmp => network%cmps%compound(n)
            nlinks = pcmp%numlinks
            call average_valstruct(valcmpstru(:,n), ST_COMPOUND, 0, nlinks, NUMVALS_CMPSTRU)
         end do
      end if
      
      ! === Bridge
      if (network%sts%numBridges > 0 .and. allocated(valbridge)) then
         if (jampi > 0) then
            call subsitute_reduce_buffer( valbridge, network%sts%numBridges*NUMVALS_BRIDGE )
         end if
         do n=1,network%sts%numBridges
            istru = network%sts%bridgeIndices(n)
            pstru => network%sts%struct(istru)
            nlinks = pstru%numlinks
            call average_valstruct(valbridge(:,n), ST_BRIDGE, istru, nlinks, NUMVALS_BRIDGE)
         end do
      end if
      ! === Culvert
      if (network%sts%numCulverts > 0 .and. allocated(valculvert)) then
         if (jampi > 0) then
            call subsitute_reduce_buffer( valculvert, network%sts%numCulverts*NUMVALS_CULVERT )
         end if
         do n=1,network%sts%numCulverts
            istru = network%sts%culvertIndices(n)
            pstru => network%sts%struct(istru)
            nlinks = pstru%numlinks
            call average_valstruct(valculvert(:,n), ST_CULVERT, istru, nlinks, NUMVALS_CULVERT)
            if (valculvert(1,n) == 0) then
               valculvert(8:NUMVALS_CULVERT,n) = dmiss
            end if
         end do
      end if
      ! === Orifice
      if (network%sts%numOrifices > 0 .and. allocated(valorifgen)) then
         if (jampi > 0) then
            call subsitute_reduce_buffer( valorifgen, network%sts%numOrifices*NUMVALS_ORIFGEN )
         end if
         do n=1,network%sts%numOrifices
            istru = network%sts%orificeIndices(n)
            pstru => network%sts%struct(istru)
            nlinks = pstru%numlinks
            call average_valstruct(valorifgen(:,n), ST_ORIFICE, istru, nlinks, NUMVALS_ORIFGEN)
         end do
      end if
      ! === Universal weir
      if( network%sts%numUniWeirs > 0 .and. allocated(valuniweir) ) then
         if (jampi > 0) then
            call subsitute_reduce_buffer( valuniweir, network%sts%numUniWeirs*NUMVALS_UNIWEIR )
         end if
         do n = 1,network%sts%numUniWeirs
            istru = network%sts%uniweirIndices(n)
            pstru => network%sts%struct(istru)
            nlinks = pstru%numlinks
            call average_valstruct(valuniweir(:,n), ST_UNI_WEIR, istru, nlinks, NUMVALS_UNIWEIR)
            if (valuniweir(1,n) == 0) then
               valuniweir(8:NUMVALS_UNIWEIR,n) = dmiss
            end if
         enddo
      endif

      ! === Dambreak
      if( jampi > 0 .and. ti_his > 0 ) then
         if( ndambreaksg > 0 .and. allocated(valdambreak) ) then
            call subsitute_reduce_buffer( valdambreak, ndambreaksg*NUMVALS_DAMBREAK )
         endif
      end if

      ! === General structure
      if( ngenstru > 0 .and. allocated(valgenstru) ) then
         if (jampi > 0) then
            call subsitute_reduce_buffer( valgenstru, ngenstru*NUMVALS_GENSTRU )
         end if

         if (network%sts%numGeneralStructures > 0) then ! new general structure
            do n = 1, ngenstru
               istru = network%sts%generalStructureIndices(n)
               pstru => network%sts%struct(istru)
               nlinks = pstru%numlinks
               call average_valstruct(valgenstru(:,n), ST_GENERAL_ST, istru, nlinks, NUMVALS_GENSTRU)
            end do
         else! Old general structure
            do n = 1, ngenstru
               call average_valstruct(valgenstru(:,n), ST_UNSET, 0, 0, 0)
               if (jampi > 0) then
                  if (valgenstru(NUMVALS_GENSTRU,n) > 1) then ! The structure lies on more than one partition
                     valgenstru(13,n) = valgenstru(13,n)/valgenstru(NUMVALS_GENSTRU,n)    ! id_genstru_openw.
                     valgenstru(14,n) = valgenstru(14,n)/valgenstru(NUMVALS_GENSTRU,n)    ! id_genstru_edgel.
                     valgenstru(9,n)  = valgenstru(9,n)/valgenstru(NUMVALS_GENSTRU,n)     ! id_genstru_cresth.
                  end if
               end if
            end do
         end if
      endif

      if( nweirgen > 0 .and. allocated(valweirgen) ) then
         if (jampi > 0) then
            call subsitute_reduce_buffer( valweirgen, nweirgen*NUMVALS_WEIRGEN )
         end if

         if (network%sts%numWeirs > 0) then ! new weir
            do n = 1,nweirgen
               istru = network%sts%weirIndices(n)
               pstru => network%sts%struct(istru)
               nlinks = pstru%numlinks
               call average_valstruct(valweirgen(:,n), ST_WEIR, istru, nlinks, NUMVALS_WEIRGEN)
            end do
         else ! old weir
            do n = 1,nweirgen
               call average_valstruct(valweirgen(:,n), ST_UNSET, 0, 0, 0)
               i = weir2cgen(n)
               if (L1cgensg(i) <= L2cgensg(i)) then  ! At least one flow link in this domain is affected by this structure.
                  valweirgen(NUMVALS_WEIRGEN,n) = 1  ! rank contains the weir.
                  valweirgen(10,n) = zcgen(3*i  )    ! id_weirgen_crestw.
                  valweirgen(9,n) = zcgen(3*i-2)     ! id_weirgen_cresth.
               end if
            end do
         end if
      endif

      if( jampi > 0 .and. ti_his > 0 ) then
         if( ngategen > 0 .and. allocated(valgategen) ) then
            call subsitute_reduce_buffer( valgategen, ngategen*NUMVALS_GATEGEN )
            do n = 1,ngategen
               if( valgategen(1,n) == 0d0 ) then
                  valgategen(2,n) = dmiss
                  valgategen(3,n) = dmiss
                  valgategen(4,n) = dmiss
                  valgategen(5,n) = dmiss
                  valgategen(7,n) = dmiss
                  valgategen(8,n) = dmiss
                  valgategen(9,n) = dmiss
               else
                  i = gate2cgen(n)
                  valgategen(3,n) = valgategen(3,n) / valgategen(1,n)
                  valgategen(4,n) = valgategen(4,n) / valgategen(1,n)
                  valgategen(5,n) = max( min( zcgen(3*i-1)-zcgen(3*i-2), valgategen(5,n)/valgategen(1,n)-zcgen(3*i-2) ), 0.0d0)  ! flow through height is always positive
                  valgategen(7,n) = valgategen(7,n) / valgategen(6,n) !id_gategen_openw
                  valgategen(8,n) = valgategen(8,n) / valgategen(6,n) !id_gategen_edgel
                  valgategen(9,n) = valgategen(9,n) / valgategen(6,n) !id_gategen_sillh
               endif
            enddo
         endif

         if( ncgensg > 0 .and. allocated(valcgen) ) then
            call subsitute_reduce_buffer( valcgen, ncgensg*NUMVALS_CGEN     )
            do n = 1,ncgensg
               if( valcgen(1,n) == 0d0 ) then
                  valcgen(2,n) = dmiss
                  valcgen(3,n) = dmiss
                  valcgen(4,n) = dmiss
               else
                  valcgen(3,n) = valcgen(3,n) / valcgen(1,n)
                  valcgen(4,n) = valcgen(4,n) / valcgen(1,n)
               endif
            enddo
         endif
         
         if( ncdamsg > 0 .and. allocated(valcdam) ) then
            call subsitute_reduce_buffer( valcdam   , ncdamsg*NUMVALS_CDAM     )
            do n = 1,ncdamsg
               if( valcdam(1,n) == 0d0 ) then
                  valcdam(2,n) = dmiss
                  valcdam(3,n) = dmiss
                  valcdam(4,n) = dmiss
               else
                  valcdam(3,n) = valcdam(3,n) / valcdam(1,n)
                  valcdam(4,n) = valcdam(4,n) / valcdam(1,n)
               endif
            enddo
         endif
         
         if( ngatesg > 0 .and. allocated(valgate) ) then
            call subsitute_reduce_buffer( valgate   , ngatesg*NUMVALS_GATE     )
            do n = 1,ngatesg
               if( valgate(1,n) == 0d0 ) then
                  valgate(2,n) = dmiss
                  valgate(3,n) = dmiss
                  valgate(4,n) = dmiss
               else
                  valgate(3,n) = valgate(3,n) / valgate(1,n)
                  valgate(4,n) = valgate(4,n) / valgate(1,n)
               endif
            enddo
         endif
      end if

      if( npumpsg > 0 .and. allocated(valpump) ) then
         if (jampi > 0) then
            call subsitute_reduce_buffer( valpump   , npumpsg*NUMVALS_PUMP     )
         end if
         do n = 1,npumpsg
            call average_valstruct(valpump(:,n), ST_UNSET, 0, 0, 0)
            
            do L = L1pumpsg(n),L2pumpsg(n)
               Lf = kpump(3,L)
               La = abs( Lf )
               jaghostexist = 0
               if( jampi > 0 ) then
                  call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
                  if ( jaghost.eq.1 ) then
                     jaghostexist = 1
                     cycle
                  end if
               endif
            end do
            if (jampi > 0) then
               valpump(6:12,n) = 0d0
            end if
            if (L1pumpsg(n) <= L2pumpsg(n) .and. jaghostexist == 0) then
               if (allocated(pumpsWithLevels)) then
                  istru = pumpsWithLevels(n)
               else
                  istru = -1
               end if
               if (istru > 0) then ! TODO: UNST-2587: once all pump code is done, remove this temp IF.
                  pstru => network%sts%struct(istru)
                  valpump(6,n) = GetPumpCapacity(pstru)
                  valpump(12,n) = sign(1,pstru%pump%direction) * valpump(2,n) ! Discharge w.r.t. pump direction (same sign as capacity)
                  valpump(7,n) = GetPumpStage(pstru)
                  if (valpump(7,n) < 0d0) then
                     valpump(7,n) = dmiss ! Set to fill value if stage is irrelevant.
                  end if
                  if (pstru%pump%direction*pstru%pump%capacity(1) > 0) then
                     valpump(11,n) = valpump(3,n) ! water level at delivery side
                     valpump(10,n) = valpump(4,n) ! water level at suction side
                  else
                     valpump(11,n) = valpump(4,n)
                     valpump(10,n) = valpump(3,n)
                  end if
                  valpump(8,n) = valpump(10,n) - valpump(11,n) ! Pump head
                  valpump(9,n) = GetPumpReductionFactor(pstru)
               end if
            end if
         enddo
         if (jampi > 0) then
            call reduce_crs(valpump(6:12,1:npumpsg),npumpsg,7)
         end if
      endif

      !update timeprev
      timprev = time1

 end subroutine structure_parameters
