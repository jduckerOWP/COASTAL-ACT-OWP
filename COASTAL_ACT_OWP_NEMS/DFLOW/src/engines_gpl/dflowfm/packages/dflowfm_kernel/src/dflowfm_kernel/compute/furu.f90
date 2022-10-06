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

! $Id: furu.f90 140637 2022-01-17 09:49:28Z kernkam $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/furu.f90 $

subroutine furu()                                   ! set fu, ru and kfs
 use m_flow                                          ! substitue u1 and q1
 use m_flowgeom
 use timers
 use m_flowtimes
 use m_alloc
 use m_partitioninfo
 use m_xbeach_data, only: ust, vst, urms, swave, Lwave
 use m_waves, only: ypar, cfwavhi, hminlw, cfhi_vanrijn, uorb
 use m_sediment
 use unstruc_channel_flow
 use m_sferic
 use m_trachy, only: trachy_resistance
 use unstruc_model, only: md_restartfile

 implicit none

 integer          :: L, Lf, n, k1, k2, kb, LL, k, itu1, Lb, Lt, itpbn, ns, i

 double precision :: bui, cu, du, du0, gdxi, ds, riep, as, gdxids
 double precision :: slopec, hup, u1L, v2, frL, u1L0, rhof, zbndun, zbndu0n, bdmwrp, bdmwrs
 double precision :: qk0, qk1, dzb, hdzb, z00  !
 double precision :: as1, as2, qtotal, width, st2, cmustr, wetdown, dpt
 double precision :: twot = 2d0/3d0, hb, h23, ustbLL, agp, vLL
 double precision :: hminlwi,fsqrtt,uorbL

 integer          :: np, L1     ! pumpstuff
 double precision :: ap, qp, vp ! pumpstuff

 double precision :: cfuhi3D, Cz    ! for bed friction

 integer          :: ierr, jaustarintsave
 double precision :: sqcfi
 logical          :: SkipDimensionChecks
 integer :: ispumpon

 hminlwi=1d0/hminlw
 fsqrtt = sqrt(0.5d0)
 call timstrt('Furu', handle_furu)

 if (kmx == 0 .or. ifixedweirscheme > 0)  then  ! original 2D coding

    !$OMP PARALLEL DO                       &
    !$OMP PRIVATE(L,k1,k2,slopec,hup,gdxi,cu,du,du0,ds,u1L,v2,itu1,frL,bui,u1L0,st2,agp)
    do L  = 1,lnx

       if (hu(L) > 0) then

          if (kmx > 0) then
             if (.not. (iadv(L) == 21 .or. iadv(L) >= 23 .and. iadv(L) <= 25) ) then  ! in 3D, only do this for weir points
                cycle
             endif
          endif

          k1  = ln(1,L) ; k2 = ln(2,L)

          slopec = 0d0
          if (L > lnx1D) then
             if (Slopedrop2D > 0) then           ! 2D droplosses at ridge points and at 2D/1D2D couplings
                if (iadv(L) == 8) then
                   hup = s0(k2) - ( min(bob(1,L), bob(2,L) ) + twot*hu(L) )
                   if (hup < 0) then
                       slopec = hup
                   else
                      hup = s0(k1) - ( min( bob(1,L), bob(2,L) ) + twot*hu(L) )
                      if (hup < 0) then
                          slopec = -hup
                      endif
                   endif
                endif
             endif
          else if (iadv(L) == 8) then            ! 1d or 1D2D droplosses, coding to avoid evaluating array iadv as long as possible,
             hup = s0(k2) - ( max(bob(1,L), bob(2,L) ) + twot*hu(L) )
             if (hup < 0) then
                slopec = hup
             else
                hup = s0(k1) - ( max( bob(1,L), bob(2,L) ) + twot*hu(L) )
                if (hup < 0) then
                    slopec = -hup
                endif
             endif
          else if (Drop1d) then            ! 1d droplosses, coding to avoid evaluating array iadv as long as possible,
             hup = s0(k2) - bob(2,L)
             if (hup < 0) then
                slopec = hup
             else
                hup = s0(k1) - bob(1,L)
                if (hup < 0) then
                    slopec = -hup
                endif
             endif
          endif

          agp = ag
          if (jahelmert > 0 .and. jsferic > 0) then
             st2  = sin(dg2rd*yu(L))**2
             agp  = 9.7803253359*(1d0+0.00193185265241*st2)/sqrt(1d0-0.00669437999013*st2)
          endif
          gdxi  = agp*dxi(L)
          cu    = gdxi*teta(L)
          du    = dti*u0(L) - adve(L) + gdxi*slopec
          ds    = s0(k2) - s0(k1)
          if (teta(L) /= 1d0) then
             du = du - (1d0-teta(L))*gdxi*ds
          endif
          du0 = du

          u1L = u0(L)

          if (jaconveyance2D >=3 .or. L <= lnx1D ) then
             v2 = 0d0
          else
             v2 = v(L)*v(L)
          endif

          if (jafrculin > 0) then
              advi(L) = advi(L) + frculin(L)/hu(L)
          endif


          itu1  = 0

10        continue

          if (jawave==3 .or. (jawave==4 .and. swave==1) .or. jawave==6 .and. .not. flowWithoutWaves) then                ! Delft3D-Wave Stokes-drift correction
              ! D3D: Compare with taubsu computation (taubot.f90) and usage (cucnp.f90/uzd.f90)
              ! A3M:ok for c01-validation_3.1.6: frL = ypar(L)*(cfuhi(L)+cfwavhi(L))*wavmu(L)/hu(L)
              if (modind < 9) then
                 frL = ypar(L)*(cfuhi(L)+cfwavhi(L)) * sqrt((u1L-ustokes(L))**2 + (v(L)-vstokes(L))**2)
              elseif (modind==9) then
                 frL = cfhi_vanrijn(L) * sqrt((u1L-ustokes(L))**2 + (v(L)-vstokes(L))**2)
              elseif (modind==10) then   ! Ruessink 2003
                 uorbL = .5d0*(uorb(k1)+uorb(k2))
                 frL = cfuhi(L)*sqrt((u1L-ustokes(L))**2 + (v(L)-vstokes(L))**2 + (1.16d0*uorbL*fsqrtt)**2)
              end if

              !bdmwrs = frL * wavmu(L)
              !bdmwrp = frL * hu(L)

              ! Bed shear due to flow and waves:
              ! A3M:ok for c01-validation_3.1.6: du = du + frL * sqrt((u1L-ustokes(L))**2 + (v(L)-vstokes(L))**2)
              !du = du0 + frL*wavmu(L)*huvli(L)
              du = du0 + frL*ustokes(L)

          else if ( ifxedweirfrictscheme > 0) then
              if (iadv(L) == 21 .or. kcu(L) == 3) then
                 call fixedweirfriction2D(L,k1,k2,frL)
              else
                 frL = cfuhi(L)*sqrt(u1L*u1L + v2)   ! g / (H.C.C) = (g.K.K) / (A.A) travels in cfu
              endif
          else if ((jaBaptist >= 2) .or. trachy_resistance) then
              frL = ( cfuhi(L) + alfav(L) )*sqrt(u1L*u1L + v2)      ! g / (H.C.C) = (g.K.K) / (A.A) travels in cfu
          else
              frL = cfuhi(L)*sqrt(u1L*u1L + v2)      ! g / (H.C.C) = (g.K.K) / (A.A) travels in cfu
          endif

          bui   = 1d0 / ( dti + advi(L) + frL )
          fu(L) = cu*bui
          ru(L) = du*bui
          u1L0  = u1L
          u1L   = ru(L) - fu(L)*ds
          itu1  = itu1 + 1
          if (huvli(L) > 1d0 .and. itu1 < 4 .and. abs( u1L-u1L0 ) > 1d-2 ) then  ! less than 1 m deep
              goto 10
          endif

       endif

    enddo
    !$OMP END PARALLEL DO   ! todo check difference

    if (npump > 0) then ! model has at least one pump link
    do np = 1,npumpsg  ! loop over pump signals, sethu
       qp    = qpump(np)
       ap    = 0d0
       vp    = 0d0
       do n  = L1pumpsg(np), L2pumpsg(np)
          k1 = kpump(1,n)
          L1 = kpump(3,n)
          L  = iabs(L1)
          hu(L) = 0d0; au(L) = 0d0
          fu(L) = 0d0; ru(L) = 0d0
          if (hs(k1) > 1d-2 .and. ispumpon(np,s1(k1)) == 1) then
             hu(L) = 1d0
             au(L) = 1d0
             ap    = ap + au(L)
             vp    = vp + vol1(k1)
          endif
       enddo
       if (qp > 0.5d0*vp/dts) then
           qp = 0.5d0*vp/dts
       endif

       if (ap > 0d0) then
          do n  = L1pumpsg(np), L2pumpsg(np)
             k1 = kpump(1,n)
             if (hs(k1) > 1d-2) then
                L1 = kpump(3,n)
                L  = iabs(L1)
                fu(L) = 0d0
                if (L1 > 0) then
                    ru(L) =  qp/ap
                else
                    ru(L) = -qp/ap
                endif
             endif
          enddo
       endif
    enddo
    end if

    call furu_structures()

 endif

 if (kmx > 0) then

    if ( jafilter.ne.0 ) then
      call comp_filter_predictor()
    end if
    if (jawave > 0 .and. .not. flowWithoutWaves ) then ! now every timestep, not only at getfetch updates
       do k = 1,ndx
          call tauwavehk(Hwav(k), Twav(k), hs(k), Uorb(k), rlabda(k), ustk(k))
       enddo
    endif

    call update_verticalprofiles()

 endif


do n  = 1, nbndu                                    ! boundaries at u points

    k2    = kbndu(2,n)
    LL    = kbndu(3,n)
    itpbn = kbndu(4,n)
    call getLbotLtop(LL,Lb,Lt)

    !Original:  !zbndun = zbndu( (n-1)*kmxd + 1 )
    if (itpbn == 4) then       ! dischargebnd
       zbndun = zbndq(n)
    else if (itpbn == 5) then  ! absgenbc
       zbndun = u1(LL)         ! set in xbeach_absgen_bc
    else                       ! other types that use alfsmo
       zbndun = zbndu( (n-1)*kmxd + 1 )
    end if

    if (alfsmo < 1d0) then
       zbndu0n = u0(LL)
       zbndun  = alfsmo*zbndun  + (1d0-alfsmo)*zbndu0n                     ! i.c. smoothing, start from 0
    endif

    if (itpbn == 8) then                             ! Criticaloutflowbnd
       if (hu(LL) > 0d0) then
          zbndun  = -sqrt(ag*( s1(k2) - min( bob(1,LL) , bob(2,LL) ) ) )
       endif
    else if (itpbn == 9) then                        ! Weiroutflowbnd 2/3h(sqrt
       if (hu(LL) > 0d0) then
          hb      = s1(k2) - min( bob(1,LL) , bob(2,LL) )
          h23     = twot*hb
          au(LL)  = twot*au(LL)
          zbndun  = -sqrt(ag*h23)
       endif
    endif

    if (Lt > Lb) then      ! true 3D
       u1(LL) = zbndun
       jaustarintsave = jaustarint
       if( jaustarint == 0 .or. jaustarint == 3 ) jaustarint = 1
       vLL = v(LL) ; v(LL) = 0d0
       call getustbcfuhi( LL,LL,ustbLL,cfuhi(LL),hdzb, z00, cfuhi3D)    ! call with Lb = LL => layer integral profile
       v(LL) = vLL
       jaustarint = jaustarintsave
       qk0 = 0d0
    endif

    do L = Lb,Lt
       fu(L) = 0d0
       ru(L) = zbndun

       if (Lt > Lb ) then
          if (jaLogprofatubndin /= 1 .and. itpbn == 3) then ! non logprof and vertical profile specified
             ru(L) = zbndu( (n-1)*kmxd + L - Lb + 1 )*min(1d0,alfsmo)
          else if (abs(u1(Lb)) > 1d-4 .and. z00 > 0d0) then
             if( jaustarint == 0 .or. jaustarint == 3 .or. jaustarint == 1 ) then
                dzb   = hu(L) + c9of1*z00
                sqcfi = (log(dzb/z00)-1d0) / vonkar
             else if( jaustarint == 2 ) then
                dzb   = hu(L)/ee + c9of1*z00
                sqcfi = ( log(dzb/z00) ) / vonkar
             else if( jaustarint == 4 ) then
                dzb   = hu(L)/ee + c9of1*z00 *0.66d0
                sqcfi = ( log(dzb/z00) ) / vonkar
             else if( jaustarint == 5 ) then
                dzb   = hu(L)
                sqcfi = ( ( 1.0d0 + c9of1 * z00 / dzb ) * log(dzb/z00+c9of1) - c9of1 * z00/dzb * log(c9of1) - 1.0d0 ) / vonkar
             endif
             qk1   = hu(L)*ustbLL*sqcfi               ! integral flux till level k
             ru(L) = (qk1 - qk0) / ( hu(L) - hu(L-1) )
             if (zbndun < 0d0) ru(L) = -1d0*ru(L)
             qk0   =  qk1
          endif
       endif

    enddo

 enddo

 call furusobekstructures()

 if (jawave==3 .or. jawave==6 .and. .not. flowWithoutWaves) then
    if (kmx==0) then
       !   add wave-induced mass fluxes on boundaries to convert euler input to GLM
       do L=Lnxi+1,Lnx
          ru(L) = ru(L) + wavmubnd(L)
       end do
    else ! to check: vertical distribution
       do L = lnxi+1,lnx
          call getLbotLtop(L,Lb,Lt)
          if (Lt<Lb) cycle
          do LL=Lb, Lt
             ru(LL) = ru(LL) + wavmubnd(LL)
          enddo
       enddo
    endif
 end if

 do i = 1, nqhbnd
    do n   = L1qhbnd(i), L2qhbnd(i)
       kb  = kbndz(1,n)
       k2  = kbndz(2,n)
       L   = kbndz(3,n)
       if (au(L)> 0d0 .and. qh_gamma(i) /= 0d0 .and. atqh_all(i) /= 0d0) then
          fu(L) = abs(q1(L)/atqh_all(i)) * qh_gamma(i)/au(L)
          ru(L) = q1(L) / au(L)
          continue
       endif
     enddo
  enddo

! BEGIN DEBUG
 if ( jampi.eq.1 ) then
!    call update_ghosts(ITYPE_U,1,Lnx,fu,ierr)
!    call update_ghosts(ITYPE_U,1,Lnx,ru,ierr)


!     call diff_ghosts(ITYPE_U,dxi)
!     call diff_ghosts(ITYPE_Sall,ucx)
 endif
! END DEBUG

 call timstop(handle_furu)

 end subroutine furu

