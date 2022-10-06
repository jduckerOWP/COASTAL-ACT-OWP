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

! $Id: znod.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/znod.f90 $

 double precision function znod(kk)                   ! get various values at flow nodes
 use m_flow
 use m_flowgeom
 use m_reduce
 use m_flowtimes ! for volerr
 use m_sediment
 use m_missing
 use m_partitioninfo
 use m_xbeach_data !, only: ee1, rr, ee1sum, H, Fx_cc, Fy_cc
 use m_transportdata
 use m_missing
 use m_observations
 use bedcomposition_module
 use precision
 use m_waves, only: waveparopt, numoptwav
 use m_flowparameters, only: ispirparopt
 use m_sferic, only:pi , rd2dg
 use m_wind, only: jawind
 use unstruc_display, only: grwhydopt


 implicit none

 common /drawthis/ ndraw(50)
 integer          :: ndraw

 integer          :: kk, k, nodval,N,L, k2
 double precision :: uu, seq(mxgr), wse(mxgr),hsk, dum, czc, taucurc,ustw2,U10,FetchL,FetchD
 real(fp)       , dimension(:,:)   , pointer :: bedtmp
 integer :: istat, jawaveswartdelwaq_local

 nodval = ndraw(28)

 if ( kk.lt.1 ) then
    znod = DMISS
    return
 end if

 if (flowWithoutWaves) then
    jawaveswartdelwaq_local = 0
 else
    jawaveswartdelwaq_local = jawaveswartdelwaq
 endif

 k = kk
 if (kmx > 0) then
    if (kplotordepthaveraged == 1) then
       call getktoplot(kk,k)
    endif
 endif

 znod = dmiss

 !if ( jampi.eq.1 ) then
 !   if ( idomain(k).ne.my_rank ) return
 !end if

 if (nodval == 2) then
    znod = s1(kk)
 else if (nodval == 3) then
    znod = bl(kk)
 else if (nodval == 4) then
    znod = ba(kk)
 else if (nodval == 5) then
    znod = a1(kk)
 else if (nodval == 6) then
    znod = vol1(k)
 else if (nodval == 7) then
    znod = s1(kk) - bl(kk)
 else if (nodval == 8) then
    znod = sqrt( ucx(k)*ucx(k) + ucy(k)*ucy(k) )
 else if (nodval == 9) then
    znod = ucx(k)
 else if (nodval == 10) then
   znod = ucy(k)
 else if (nodval == 11) then
    if (jasal > 0) znod = constituents(isalt, k)
 else if (nodval == 12) then
    if (jatem > 0) then
       if (jafahrenheit == 0) then
          znod = constituents(itemp,k)
       else
          znod = 32d0 + (9d0/5d0)*constituents(itemp,k)
       endif
    endif
 else if (nodval == 13) then
    if (jased > 0 .and. .not. stm_included) then
       znod = sed( jgrtek, k )
    else if (jagrw > 0) then
       znod = sgrw1( kk )
    endif
 else if (nodval == 14) then
    if (hs(k) > 0) then
       znod = sqrt( ucx(k)*ucx(k) + ucy(k)*ucy(k) ) / sqrt(ag*hs(k)) ! Froude
    else
       znod = 0d0
    endif
 else if (nodval == 15) then
    znod = kk
 else if (nodval == 16) then
    znod = nd(kk)%lnx
 else if (nodval == 17) then
    znod = kcs(kk)  !  voldhu(kk) - vol1(kk)
 else if (nodval == 18) then
    znod = squ(k)
 else if (nodval == 19) then
    znod = sqi(k)
 else if (nodval == 20) then
    znod = sqi(k) - squ(k)
 else if (nodval == 21) then
    znod = qw(k)/a1(kk)
 else if (nodval == 22) then
    if (jased>0) then
       call getequilibriumtransportrates(kk, seq, wse, mxgr, hsk)
       znod = seq(jgrtek)
    endif
 else if (nodval == 23) then
    znod = qin(k) ! turkinepsws(1,k)
 else if (nodval == 24) then
    if (mxgr > 1 .and. jaceneqtr == 1) znod = grainlay(jgrtek,kk)
 else if (nodval == 25 .and. kmx > 0) then
    znod = ktop(kk) - kbot(kk) + 1
 else if (nodval == 26) then
    if (squ(k) > 0d0 .and. vol1(k) > 0d0 ) then
       znod = vol1(k) /squ(k)
     else
       znod = dmiss
     endif
else if (nodval == 27) then
    if (kmx>1) znod = vicwws(k)
 ! 28 = substi/cg
 else if (nodval == 29) then
    if ( allocated(tidep) ) then
       if ( ubound(tidep,1).eq.2 ) then
          znod = tidep(2,kk)
       else
          znod = tidep(1,kk)
       end if
   end if
!     znod = plotlin(kk)
 else if (nodval == 30) then
    znod = dt_max
    do k = kbot(kk), ktop(kk)
       znod = min(znod, vol1(k) / max( squ(k), eps10) )
    enddo
 else if (nodval == 31) then
    if (japatm > 0) znod = patm(kk)
 else if (nodval == 32) then
    if (numlimdt(kk) > 0) znod = numlimdt(kk)
 else if (nodval == 33) then
   ZNOD = ( ucx(k)*ucx(k) + ucy(k)*ucy(k) ) / (2d0*ag)
   znod = u1(min(k,lnx))*u1(min(k,lnx)) / (2d0*ag)
   znod = znod + s1(kk)

   plotlin(kk) = znod

 else if (nodval == 34) then
    znod = volerror(k)
    !if (abs(znod) > 1) then
    !   znod = 0d0
    !endif
 else if (nodval == 35) then

    znod = rho(k) ! sam0(k) !  kktop(kk) - kbot(kk) + 1

 else if (nodval == 36) then

    znod = dt_max
    do k = kbot(kk), ktop(kk)
       if (squ(k) > eps10) then
          znod = min(znod, cflmx*vol1(k)/squ(k))
       endif
    enddo

 else if (nodval == 37) then

    if (Soiltempthick > 0 .and. jatem > 0) then
       znod = tbed(kk)
    else
       znod = same(k)
    endif

 else if (nodval == 38) then

    znod = zws(k) - zws(k-1)

 else if (nodval == 39) then

    call gettau(kk, znod, czc, jawaveswartdelwaq_local)

 else if (nodval == 40) then

    znod = rain(kk)

 else if (nodval == 41 .and. jatem > 0) then
    znod = rhum(kk)
 else if (nodval == 42 .and. jatem > 0) then
    znod = tair(kk)
 else if (nodval == 43 .and. jatem > 0) then
    znod = clou(kk)
 else if (nodval == 44 .and. jatem > 0 .and. allocated(qrad)) then
    znod = qrad(kk)
 else if (nodval .eq. 45 .and. NUMCONST.gt.0 ) then
    if ( iconst_cur.gt.0 .and. iconst_cur.le.NUMCONST ) then
       znod = constituents(iconst_cur,k)
    end if
 else if (nodval == 46) then
    znod =  turkinepsws(1,k)
 else if (nodval == 47 .and. (jagrw > 0 .or. jadhyd > 0)) then
    select case (grwhydopt)
    case (1) ! Ground water pressure
       if (jagrw > 0) then
          if (infiltrationmodel == 1) then
             znod = sgrw1(k)
          else
             znod = pgrw(kk)
          endif
       end if
    case (4) ! Infiltration capacity
       if (infiltrationmodel == DFM_HYD_INFILT_CONST .or. infiltrationmodel == DFM_HYD_INFILT_HORTON) then
          znod = infiltcap(kk)*1d3*3600d0 ! m/s -> mm/hr
       end if
   case (6) ! Interception layer thickness
      if (interceptionmodel == DFM_HYD_INTERCEPT_LAYER) then
         znod = InterceptThickness(kk)
      end if
   case (7) ! Interception layer water depth       (m)
      if (interceptionmodel == DFM_HYD_INTERCEPT_LAYER) then
         znod = InterceptHs(kk)
      end if
   case (8) ! Potential evaporation            (mm/hr)
      if (jadhyd == 1) then
         znod = PotEvap(kk)*1d3*3600d0 ! m/s -> mm/hr
      end if
   case (9) ! Actual evaporation open water    (mm/hr)
      if (jadhyd == 1) then
         znod = ActEvap(kk)*1d3*3600d0 ! m/s -> mm/hr
      end if
    end select

 else if (nodval == 48) then
   if (nonlin >= 2) then
      znod = a1m(kk)
   else if (japure1D > 0) then ! visualise
      znod = uc1d(kk)
   else if (kmx > 0) then 
      znod = kmxn(kk)
   endif
 else if (nodval == 49) then
    if (nshiptxy > 0) then
       znod = zsp(kk)
    endif
 else if (nodval == 50) then
    if (janudge > 0) then
       znod = 0d0
       if ( nudge_rate(kk).gt.0d0 ) then
         znod = 1d0/nudge_rate(kk)
       endif
    else if (nshiptxy > 0) then
       znod = v1ship(kk)
    endif

 else if (nodval == numoptwav .and. jawave > 0 .and. .not. flowWithoutWaves) then
    if (jawave == 1 .or. jawave == 2) then
      select case (waveparopt)
        case(1)
                znod = Hwav(kk)
        case(2)
                znod = Rlabda(kk)
        case(3)
                znod = Twav(kk)
        case(4)
                znod = Uorb(kk)
        case(5)
                call gettau2(kk,taucurc,czc,ustw2,jawaveswartdelwaq)
                znod = sqrt(ustw2)            !ustw
        case(6)
                call gettau2(kk,taucurc,czc,ustw2,jawaveswartdelwaq)
                znod = sqrt(taucurc/rhomean)  !ustw+c
        case(7)
                call gettau2(kk,taucurc,czc,ustw2,jawaveswartdelwaq)
                znod = taucurc                ! taus to Delwaq
        case(8)
                znod = ustk(kk)               ! Ustokes
        case(9)
                call getfetch(kk,U10,FetchL,FetchD)
                znod = FetchL
        case(10)
                call getfetch(kk,U10,FetchL,FetchD)
                znod = FetchD
      end select

    else
     call wave_makeplotvars()
     select case (waveparopt)
       case (1)
          if (jawave.ne.4) then
             znod = Hwav(kk) ! fwx%hrms(kk)
          else
             znod = H(kk)
          endif
       case (2)
          if (jawave.ne.4) then
             znod = Twav(kk) ! fwx%tp(kk)
          elseif (windmodel.eq.0) then
             znod = Trep
          else
             znod = tt1(itheta_view,kk)
          endif
       case (3)
          znod = taus(kk)
       case (4)
          znod = fwav_mag(kk)
       case (5)
          znod = ust_mag(k)
       case (6)
          if (jawave.ne.4) then
             znod = dmiss
          else
             znod = Fx_cc(kk)
          endif
       case (7)
          if (jawave.ne.4) then
             znod = dmiss
          else
             znod = Fy_cc(kk)
          endif
       case (8)
          if (jawave.ne.4) then
             znod = dmiss
          else
             znod = taux_cc(kk)
          endif
       case (9)
          if (jawave.ne.4) then
             znod = dmiss
          else
             znod = tauy_cc(kk)
          endif
       case (10)
          if (jawave.ne.4) then
             znod = dmiss
          else
             znod = ustx_cc(kk)
          endif
       case (11)
          if (jawave.ne.4) then
             znod = dmiss
          else
             znod = usty_cc(kk)
          endif
       case (12)
          znod = ee1(itheta_view,kk)
       case (13)
          znod = rr(itheta_view,kk)
       case (14)
          if (jawave.ne.4) then
             znod = dmiss
          else
             znod = uorb(kk)
          endif
       case (15)
          if (jawave.ne.4) then
             znod = dmiss
          else
             znod = D(kk)
          endif
       case (16)
          if (jawave.ne.4) then
             znod = dmiss
          else
             znod = DR(kk)
          endif
       case (17)
          if (jawave.ne.4) then
             znod = dmiss
          else
             znod = R(kk)
          endif
       case (18)
          if (jawave.ne.4) then
             znod = dmiss
          else
             znod = Sxx(kk)
          endif
       case (19)
          if (jawave.ne.4) then
             znod = dmiss
          else
             znod = Syy(kk)
          endif
       case (20)
          if (jawave.ne.4) then
             znod = dmiss
          else
             znod = Sxy(kk)
          endif
       case (21)
          if (jawave.ne.4) then
             znod = dmiss
          else
             znod = kwav(kk)
          endif
       case (22)
          if (jawave.ne.4) then
             znod = dmiss
          else
             znod = mod(270d0 - phiwav(kk),360d0)
          endif
       case (23)
          if (jawave.ne.4) then
             znod = dmiss
          else
             znod = dhsdx(kk)
          endif
       case (24)
          if (jawave.ne.4) then
             znod = dmiss
          else
             znod = dhsdy(kk)
          endif
       case(25)
          if (jawave .ne. 4) then
             znod = dmiss
          else if (jawind>0 .and. jawsource>0) then
             znod = wsorE(itheta_view, kk)
          end if
       case(26)
           if (jawave .ne. 4 ) then
               znod = dmiss
           else
               znod = sigt(itheta_view,kk)
           end if
       case(27)
           if (jawave .ne. 4 ) then
               znod = dmiss
           elseif (windmodel.eq.0) then
               znod = cgwav(kk)
           else
               znod = cgwavt(itheta_view,kk)
           end if
       case(28)
          if (jawave == 1 .or. jawave == 2) then
             znod = fetch(1,kk)
          else
             znod = dmiss
          end if
       case(29)
           if (jawave.ne.4) then
               znod = dmiss
           else
               znod = dtheta * egradcg(itheta_view,kk)
           endif
       case(30)
           if (jawave.ne.4) then
               znod = dmiss
           else
               znod = SwT(kk)
           endif
       case(31)
           if(jawave.ne.4) then
               znod = dmiss
           else
               znod = SwE(kk)
           endif
       case(32)
           if(jawave.ne.4) then
               znod = dmiss
           else
               znod = horadvec(itheta_view,kk)
           endif
       case(33)
           if(jawave.ne.4) then
               znod = dmiss
           else
               znod = horadvec2(itheta_view,kk)
           endif
       case(34)
           if(jawave.ne.4) then
               znod = dmiss
           else
               znod = ma(itheta_view,kk)
           endif


     end select

    endif

 else if (nodval == numoptsf .and. jasecflow > 0) then
    select case (ispirparopt)
       case (1)
          if ( jasecflow > 0 ) then
             znod = spircrv(kk)
          else
             znod = bz(kk)
          endif
       case (2)
          znod = spirint(kk)
       case (3)
          znod = sqrt( spirfx(kk) * spirfx(kk)+ spirfy(kk) * spirfy(kk) )
       end select
  else if (nodval == numoptsed .and. stm_included) then
      select case (sedparopt)
         case (1)
            znod = mtd%blchg(kk)
         case (2)
            dum = 0d0
            do l = 1, stmpar%lsedsus
               dum = dum + sedtra%sourse(kk,l)
            end do
            znod = dum
         case(3)
            dum = 0d0
            do l = 1, stmpar%lsedsus
               dum = dum + sedtra%sinkse(kk,l)
            end do
            znod = dum
      end select
 end if
 end function znod
