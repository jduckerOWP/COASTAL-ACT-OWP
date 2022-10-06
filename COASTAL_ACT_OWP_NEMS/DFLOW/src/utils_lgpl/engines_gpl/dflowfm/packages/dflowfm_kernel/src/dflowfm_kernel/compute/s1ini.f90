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

! $Id: s1ini.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/s1ini.f90 $

 subroutine s1ini()                           ! links in continuity eq.
 use m_flow
 use m_flowgeom
 use m_flowtimes
 use m_reduce
 use m_ship
 use m_transport, only : constituents, itemp
 use m_hydrology_data, only : jadhyd, ActEvap, interceptionmodel, InterceptThickness, InterceptHs, DFM_HYD_INTERCEPT_LAYER
 use m_mass_balance_areas
 use m_partitioninfo
 implicit none

 integer          :: L, k1, k2, k, kb, n, LL, kk, kt, idim, imba
 double precision :: aufu, auru, tetau
 double precision :: zb, dir, ds, qhs, hsk, buitje, Qeva_ow, Qeva_icept, Qrain, Qicept, Qextk, aloc
 logical :: isGhost

 buitje = 0.013d0/300d0                                      ! 13 mm in 5 minutes

 bb = 0d0 ; ccr = 0d0 ; dd = 0d0

 if (jagrw > 0 .or. numsrc > 0 .or. infiltrationmodel /= DFM_HYD_NOINFILT .or. nshiptxy > 0) then
    jaqin = 1
 endif

 if (jatem > 0) then
     if (jatem > 1) then
        heatsrc = heatsrc0                                   ! heatsrc0 established in heatu at interval usertimestep
     else
        heatsrc = 0d0                                        ! just prior to setsorsin that may add to heatsrc
     endif
 endif

 if (jaqin > 0) then                                         ! sources and sinks through meteo

    qin = 0d0 ; qinrain = 0d0; qinrainground = 0d0; qouteva = 0d0; qoutevaicept = 0d0; qinlat(1:2) = 0d0 ; qoutlat(1:2) = 0d0; qinext(1:2) = 0d0 ; qoutext(1:2) = 0d0
    if (jarain > 0) then
       if (rainuni > 0d0) then
          rain     = rainuni*24d0                             ! mm/hr  => mm/day
       endif

       do k = 1,ndxi
          Qrain = rain(k)*bare(k)*1d-3/(24d0*3600d0)          ! mm/day => m3/s
          if (Qrain  > 0) then
             qinrain = qinrain + Qrain                        ! rain can be pos or neg, to allow for prescribed evaporation
          else if (hs(k) > epshu) then
             Qrain   = - min(0.5d0*vol1(k)/dts , -Qrain)
             qouteva = qouteva - Qrain
          else
             Qrain   = 0d0
          endif

          if (Qrain > 0d0 .and. interceptionmodel == DFM_HYD_INTERCEPT_LAYER) then                    ! Is there rainfall AND interception?
             Qicept = min(Qrain, dti*bare(k)*(InterceptThickness(k) - InterceptHs(k)))
             InterceptHs(k) = InterceptHs(k) + dts*Qicept/bare(k)
          else
             Qicept = 0d0
          endif
          qin(k) = Qrain - Qicept
          if (Qrain - Qicept > 0) then
             qinrainground = qinrainground + Qrain - Qicept
          end if

          if (jamba > 0) then
             imba = mbadefdomain(k)
             if (imba > 0) then
                if (Qrain > 0) then
                   mbaflowraineva(1,imba) = mbaflowraineva(1,imba) + Qrain*dts
                else
                   mbaflowraineva(2,imba) = mbaflowraineva(2,imba) - Qrain*dts
                endif
             endif
          endif
       enddo
    endif

    if (jaevap > 0) then                                      ! computed evaporation is always positive, must be extracted
       do k = 1,ndxi
          if (hs(k) > epshu) then
             ! Calculates the actual evaporation rate from surface water.
             ! It is capped by the available water plus the incoming rain calculated in the code above.
             ! qin is used explicitly in the following , therefore an additional safety factor of 0.5
             ! is introduced to prevent negative water depths.
             Qeva_ow = -min(0.5d0*vol1(k)*dti + qin(k), -evap(k)*bare(k))
             if (jadhyd == 1) then ! TODO: this is can be removed once jaevap and jadhyd have been merged
                ActEvap(k) = -Qeva_ow/bare(k) ! m s-1
             endif
             qin(k)  = qin(k)  + Qeva_ow
             qouteva = qouteva - Qeva_ow
             if (jamba > 0) then
                imba = mbadefdomain(k)
                if (imba > 0) then
                   mbafloweva(imba) = mbafloweva(imba) - Qeva_ow*dts
                endif
             endif
          endif
          if (interceptionmodel == DFM_HYD_INTERCEPT_LAYER) then
             if (InterceptHs(k) > epshu) then
                ! Calculates the actual evaporation rate from intercepted water.
                ! No safety factor is needed here, since no horizontal fluxes are present.
                ! Basic evaporation from interception occurs at the same potential rate as the surface water.
                Qeva_icept = -min(dti*InterceptHs(k)*bare(k), -evap(k)*bare(k))
                InterceptHs(k) = InterceptHs(k) + Qeva_icept/bare(k)*dts
                qoutevaicept = qoutevaicept - Qeva_icept
          endif
          endif
       enddo
    endif

    if (jaQext > 0) then
       do k = 1,ndxi
          if (k <= ndx2d) then
             idim = 2
          else
             idim = 1
          end if
          if (qext(k) > 0) then ! inflow is always possible
             Qextk = qext(k)                                               ! Qext can be pos or neg
             qinext(idim) = qinext(idim) + Qextk
          else if (hs(k) > epshu) then
             Qextk = - min(0.5d0*vol1(k)/dts , -qext(k))
             qoutext(idim) = qoutext(idim) - Qextk
          else ! (almost) no water
             Qextk = 0.0d0
          endif
          qextreal(k) = Qextk
          qin(k) = qin(k) + Qextk
       enddo
    end if

    if (numlatsg > 0) then

       ! First accumulate all lateral discharges per grid cell
       QQLat(1:ndx) = 0d0
       do n = 1,numlatsg
          do k1=n1latsg(n),n2latsg(n)
             k = nnlat(k1)
             if (k > 0) then
                QQLat(k) = QQLat(k) + QPlat(n)*ba(k)/baLat(n)
             end if
          end do
       end do

       ! Now, handle the total lateral discharge for each grid cell
       do k = 1,ndxi
          if (k <= ndx2d) then
             idim = 2
          else
             idim = 1
          end if

          !DIR$ FORCEINLINE
          isGhost = is_ghost_node(k)

          if (QQLat(k) > 0) then
             if (.not. isGhost) then ! Do not count ghosts in mass balances
                qinlat(idim) = qinlat(idim) + QQLat(k)                        ! Qlat can be pos or neg
             end if
          else if (hs(k) > epshu) then
             QQlat(k) = - min(0.5d0*vol1(k)/dts , -QQlat(k))
             if (.not. isGhost) then
                qoutlat(idim) = qoutlat(idim) - QQlat(k)
             end if
          else
             QQlat(k) = 0d0
          endif
          qin(k) = qin(k) + QQlat(k)
       enddo
    endif

    if (jarain > 0 .or. jaevap > 0 .or. jaQext > 0) then ! TODO: Qlat not here?
       do k  = 1,ndxi
          kt = ktop(k)
          if (kmx > 0) then
             qin(kt) = qin(kt) + qin(k)
          endif
          if (jatem >= 1) then
             if (qin(kt) > 0) then
                if (jatem > 1) then
                   heatsrc(kt) = heatsrc(kt) + qin(kt)*tair(k)                   ! rain has temp of air time varying specified
                else if (jatem == 1) then
                   heatsrc(kt) = heatsrc(kt) + qin(kt)*backgroundairtemperature  ! or constant
                endif
             else
                heatsrc(kt) = heatsrc(kt) + qin(kt)* constituents(itemp,kt)      ! extract top layer temp
             endif
          endif
       enddo
    endif

    if (jagrw > 0 .or. infiltrationmodel /= DFM_HYD_NOINFILT) then
       call setgrwflowexpl()                                 ! add grw-flow exchange to the qin array
    endif

    if (numsrc > 0) then
       call setsorsin()                                      ! add sources and sinks
    endif

    if (nshiptxy > 0) then
       ! qin = qin - qinship
    endif

    qincel = 0d0 ; qoutcel = 0d0

    do k = 1,ndxi
       if (qin(k) > 0d0) then
              dd(k)  = qin(k)
       else if (qin(k) < 0d0) then

          hsk = s0(k) - bl(k)
          if (a1(k) > 0.0) then
             ds  = -dts*qin(k)/a1(k)                            ! altijd minder dan daling bij niet-lin volumes
             aloc = a1(k)
          else
             ds  = -dts*qin(k)/ba(k)                            ! altijd minder dan daling bij niet-lin volumes
             aloc = ba(k)
          endif
          if (kfs(k) <= 0) then                              ! niet in matrix
             if (ds  < hsk) then                             ! er is genoeg
                s1(k) = s0(k) - ds
             else                                            ! leeg
                s1(k) = bl(k) ; ds = hsk
             endif
             qin(k) = -ds*aloc/dts

          else if (hsk > 0d0) then                            ! all explicit
             dd(k)  = -min( vol1(k)/dts, abs(qin(k) ) )
             qin(k) = dd(k)

          else                                               ! er is te weinig
             dd(k)  = 0 ; qin(k) = 0                         ! => wachten tot kfs=0 en expliciet scheppen
          endif
       else
          dd(k) = 0
       endif
       !DIR$ FORCEINLINE
       isGhost = is_ghost_node(k)
       if (.not. isGhost) then ! Do not count ghosts in mass balances
         qincel = qincel + qin(k)
       end if
    enddo
 endif

 if (kmx < 1) then ! original 2D coding

    do L = 1,lnx
       if (hu(L) > 0) then
           tetau       = teta(L)*au(L)
           aufu        = tetau*fu(L)
           k1 = ln(1,L); k2 = ln(2,L)
           bb(k1)      = bb(k1)      + aufu
           bb(k2)      = bb(k2)      + aufu
           ccr(Lv2(L)) = ccr(Lv2(L)) - aufu

           auru        = tetau*ru(L) + (1d0 - teta(L))* au(L)*u0(L)  !     q1(L)
           dd(k1)      = dd(k1) - auru
           dd(k2)      = dd(k2) + auru
       endif
    enddo
    !
 else

    do LL = 1,lnx
       if (hu(LL) > 0) then
          k1 = ln(1,LL) ; k2 = ln(2,LL)

          do L = Lbot(LL), Ltop(LL)

             if (hu(L) > 0) then
                tetau        = teta(LL)*au(L)

                aufu         = tetau*fu(L)
                bb(k1)       = bb(k1)       + aufu
                bb(k2)       = bb(k2)       + aufu
                ccr(Lv2(LL)) = ccr(Lv2(LL)) - aufu

                auru         = tetau*ru(L) + (1d0 - teta(LL))*au(L)*u0(L)  !     q1(L)
                dd(k1)       = dd(k1) - auru
                dd(k2)       = dd(k2) + auru
             endif
          enddo
       endif

    enddo


 endif

 if (nonlin > 0) then
    ccrsav = ccr
 endif

 end subroutine s1ini
