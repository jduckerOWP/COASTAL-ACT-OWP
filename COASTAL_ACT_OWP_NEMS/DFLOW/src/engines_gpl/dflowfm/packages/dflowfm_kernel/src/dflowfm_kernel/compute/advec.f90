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

! $Id: advec.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/advec.f90 $

 subroutine advec()                                  ! advection, based on u0, q0 24
 use m_flowtimes
 use m_flowgeom
 use m_flow
 use m_partitioninfo
 use m_fixedweirs
 use m_sferic
 use unstruc_channel_flow, only: network

 implicit none

 ! locals
 double precision                  :: unormal        ! function unormal
 double precision                  :: dif            ! averaged waterdepth at flow link (m)
 integer                           :: L, k1, k2      ! link, nd1, nd2
 integer                           :: k12            ! nd1  or nd2
 integer                           :: k34            ! nod1 or nod2
 double precision                  :: ul1, ul2       ! just testing
 double precision                  :: uup            ! cell centered upwind u(L)
 double precision                  :: vup            ! cell centered upwind v(L) determines whether to use corner up or dwn in vdudy
 double precision                  :: ucnup          ! corner based  upwind u(L)
 double precision                  :: vcn3, vcn4, vcnu, qx
 double precision                  :: v12            ! Wenneker control volume (m3)
 double precision                  :: v12t, v1t, v2t ! time derivative of control volume (m3/s)
 double precision                  :: advil          ! local advi
 double precision                  :: advel          ! local adve

 double precision                  :: qu1            ! Flux times advection velocity node 1 (m4/s2)
 double precision                  :: qu2            ! idem                          node 2
 double precision                  :: qu1a           ! Flux times advection velocity node 1 (m4/s2)
 double precision                  :: qu2a           ! idem                          node 2
 double precision                  :: qu12a          ! both a
 double precision                  :: uqcxl          !
 double precision                  :: uqcyl          !
 double precision                  :: qu12, aa       ! both
 double precision                  :: cs, sn

 double precision                  :: QucWen         ! Sum over links of Flux times upwind cell centre velocity (m4/s2), do not include own link
 double precision                  :: QucPer         ! idem, include own link
 double precision                  :: QucPer3D       ! idem, include own link
 double precision                  :: QucPerpure1D   ! idem, include own link
 !double precision                  :: QucWeni        ! idem, only incoming
 double precision                  :: QucPeri        ! idem, inly incoming         nb: QucPeripiaczek is a subroutine
! double precision                  :: QunPeri

 double precision                  :: QucPerq1       ! ..
 double precision                  :: QucPercu       ! testing center differences
 double precision                  :: QufPer         ! testing adv of face velocities instead of centre upwind velocities

 !double precision                  :: Qucnu          ! = original of QucPer


 double precision                  :: visc           ! eddy viscosity term

 integer                           :: isg, jcheck, iadvL, ierr
 integer                           :: m, mu, md, mdd, iad, n, kk, kb
 double precision                  :: qxm, qxmu, uam, uamu, uamd, qxmd, du
 double precision                  :: vv1, vv2, dv1, dv2, quk, que
 double precision                  :: ucxku, ucyku, ai, ae, abh, vu1Di, volu, volui, hh, huvL, baik1, baik2
 double precision                  :: ucin, fdx, ql, qucx, qucy, ac1, ac2, uqn, qn, rhoinsrc, dzss, qnn
 integer                           :: LL,LLL,LLLL, Lb, Lt, Lay, i

 integer                           :: ierror, ku, kd, k, nfw, kt
 integer                           :: n12

 double precision                  :: quk1(3,kmxx), quk2(3,kmxx), volukk(kmxx)   ! 3D for 1=u, 2=turkin, 3=tureps

 integer                           :: kt1, kt2, n1, n2, kb1, kb2, Ltx0, ktx01, ktx02 , ktx1 , ktx2, Ltx, L1, ksb, kst
 double precision                  :: sigu, alf, bet1, bet2, hs1, hs2, vo1, vo2, zz1, zz2, econsfac
 double precision                  :: tol=1d-4 , sl, dzu, dzk, du1, du2, dux, duy, expl

 double precision                  :: quuk1(0:kmxx), quuk2(0:kmxx), volk1(0:kmxx), volk2(0:kmxx), sqak1(0:kmxx), sqak2(0:kmxx)
 double precision                  :: quuL1(0:kmxx), quuL2(0:kmxx), volL1(0:kmxx), volL2(0:kmxx), sqaL1(0:kmxx), sqaL2(0:kmxx)
 double precision                  :: sigk1(0:kmxx), sigk2(0:kmxx), siguL(0:kmxx)

 double precision,        external :: lin2nodx, lin2nody
 double precision,        external :: nod2linx, nod2liny
 double precision,        external :: dlimiter, dslim

 japiaczek33 = 0

 if (ifixedweirscheme >= 3 .and. ifixedweirscheme <= 5) then
    do L  = 1,lnxi
       if (iadv(L) == 21) then
          if (u0(L) > 0) then
             kd = ln(2,L) ; ku = ln(1,L)
          else
             kd = ln(1,L) ; ku = ln(2,L)
          endif
          call getucxucyweironly ( kd, ucx(kd), ucy(kd), ifixedweirscheme )
          call getucxucyweironly ( ku, ucx(ku), ucy(ku), ifixedweirscheme )
       endif
    enddo
 endif

 if (jabarrieradvection == 2) then
    do n = 1,ngatesg
       do L = L1gatesg(n), L2gatesg(n)
          LL = kgate(3,L) ; LL = iabs( LL )
          kd = ln(1,LL) ; ku = ln(2,LL)
          call getucxucybarrierzero ( LL, kd, ucx(kd), ucy(kd) )
          call getucxucybarrierzero ( LL, ku, ucx(ku), ucy(ku) )
       enddo
    enddo
    do n = 1,ngategen
       i = gate2cgen(n)
       do L = L1cgensg(i), L2cgensg(i)
          LL = kcgen(3,L) ; LL = iabs( LL )
          kd = ln(1,LL) ; ku = ln(2,LL)
          call getucxucybarrierzero ( LL, kd, ucx(kd), ucy(kd) )
          call getucxucybarrierzero ( LL, ku, ucx(ku), ucy(ku) )
       enddo
    enddo
 endif

 call sethigherorderadvectionvelocities()

 uqcx = 0d0 ; uqcy = 0d0 ; sqa = 0d0

 if (kmx == 0) then

   if (jasfer3d == 1) then

      do L = Lnx,1,-1
          k1 = ln(1,L) ; k2 = ln(2,L)
          qL = qa(L)
          uqcx(k1) = uqcx(k1) + qL*lin2nodx(L,1,ucxu(L),ucyu(L))
          uqcx(k2) = uqcx(k2) - qL*lin2nodx(L,2,ucxu(L),ucyu(L))
          uqcy(k1) = uqcy(k1) + qL*lin2nody(L,1,ucxu(L),ucyu(L))
          uqcy(k2) = uqcy(k2) - qL*lin2nody(L,2,ucxu(L),ucyu(L))
          sqa (k1) = sqa (k1) + ql
          sqa (k2) = sqa (k2) - ql
       enddo

   else

       do L = Lnx,1,-1
          k1 = ln(1,L) ; k2 = ln(2,L)
          qL = qa(L)
          uqcx(k1) = uqcx(k1) + qL*ucxu(L)
          uqcx(k2) = uqcx(k2) - qL*ucxu(L)
          uqcy(k1) = uqcy(k1) + qL*ucyu(L)
          uqcy(k2) = uqcy(k2) - qL*ucyu(L)
          sqa (k1) = sqa (k1) + ql
          sqa (k2) = sqa (k2) - ql
       enddo

    endif

 else

    do LL = Lnx,1,-1
       Lb = lbot(LL) ; Lt = ltop(LL)
       do L = Lb, Lt
          k1 = ln(1,L) !; k1 = min(k1, ktop(ln(1,LL) ) )
          k2 = ln(2,L) !; k2 = min(k2, ktop(ln(2,LL) ) )
          qL = qa(L)
          if (jasfer3d == 1) then
             uqcx(k1) = uqcx(k1) + qL*lin2nodx(LL,1,ucxu(L),ucyu(L))
             uqcx(k2) = uqcx(k2) - qL*lin2nodx(LL,2,ucxu(L),ucyu(L))
             uqcy(k1) = uqcy(k1) + qL*lin2nody(LL,1,ucxu(L),ucyu(L))
             uqcy(k2) = uqcy(k2) - qL*lin2nody(LL,2,ucxu(L),ucyu(L))
          else
             uqcx(k1) = uqcx(k1) + qL*ucxu(L)
             uqcx(k2) = uqcx(k2) - qL*ucxu(L)
             uqcy(k1) = uqcy(k1) + qL*ucyu(L)
             uqcy(k2) = uqcy(k2) - qL*ucyu(L)
          endif
          sqa (k1) = sqa (k1) + ql
          sqa (k2) = sqa (k2) - ql
       enddo
    enddo

 endif

 if (javau >= 6) then ! 3D checkerboard pepare explicit node based vertical advection
    if (jarhoxu == 0) then
       do kk = 1,ndxi
          call getkbotktop(kk,kb,kt)
          do k = kb, kt-1
             if ( qw(k) > 0d0) then
                 uqcx(k+1) = uqcx(k+1) - qw(k)*ucx(k)
                 uqcx(k  ) = uqcx(k  ) + qw(k)*ucx(k)
                 uqcy(k+1) = uqcy(k+1) - qw(k)*ucy(k)
                 uqcy(k  ) = uqcy(k  ) + qw(k)*ucy(k)
                 if (javau == 7 .and. k > kb ) then
                    dzu =  zws(k) - zws(k-2)      ! 2*dz of upwind face
                    if ( dzu > tol) then
                       dzk =  zws(k+1) - zws(k-1) ! 2*dz of this face
                       sl  =  dzk/dzu
                       du2 = (ucx(k+1) - ucx(k)   )
                       du1 = (ucx(k )  - ucx(k-1) )*sl
                       ! dux =  0.5d0*dlimiter(du1,du2,4)
                       dux =  0.5d0*dslim(du1,du2,4)
                       du2 = (ucy(k+1) - ucy(k)   )
                       du1 = (ucy(k )  - ucy(k-1) )*sl
                       ! duy =  0.5d0*dlimiter(du1,du2,4)
                       duy =  0.5d0*dslim(du1,du2,4)
                       uqcx(k+1) = uqcx(k+1) - qw(k)*dux
                       uqcx(k  ) = uqcx(k  ) + qw(k)*dux
                       uqcy(k+1) = uqcy(k+1) - qw(k)*duy
                       uqcy(k  ) = uqcy(k  ) + qw(k)*duy
                    endif
                 endif
             else if ( qw(k) < 0d0) then
                 uqcx(k+1) = uqcx(k+1) - qw(k)*ucx(k+1)
                 uqcx(k  ) = uqcx(k  ) + qw(k)*ucx(k+1)
                 uqcy(k+1) = uqcy(k+1) - qw(k)*ucy(k+1)
                 uqcy(k  ) = uqcy(k  ) + qw(k)*ucy(k+1)
                 if (javau == 7 .and. k < kt-1 ) then
                    dzu =  zws(k+2) - zws(k)      ! 2*dz of upwind face
                    if ( dzu > tol) then
                       dzk =  zws(k+1) - zws(k-1) ! 2*dz of this face
                       sl  =  dzk/dzu
                       du2 = (ucx(k)   - ucx(k+1) )
                       du1 = (ucx(k+1) - ucx(k+2) )*sl
                       ! dux =  0.5d0*dlimiter(du1,du2,4)
                       dux =  0.5d0*dslim(du1,du2,4)
                       du2 = (ucy(k)   - ucy(k+1) )
                       du1 = (ucy(k+1) - ucy(k+2) )*sl
                       ! duy =  0.5d0*dlimiter(du1,du2,4)
                       duy =  0.5d0*dslim(du1,du2,4)

                       uqcx(k+1) = uqcx(k+1) - qw(k)*dux
                       uqcx(k  ) = uqcx(k  ) + qw(k)*dux
                       uqcy(k+1) = uqcy(k+1) - qw(k)*duy
                       uqcy(k  ) = uqcy(k  ) + qw(k)*duy
                    endif
                 endif

             endif
             sqa(k+1) = sqa(k+1) - qw(k)
             sqa(k  ) = sqa(k  ) + qw(k)
          enddo
       enddo
    else
       do kk = 1,ndxi
          do k = kbot(kk), ktop(kk)-1
             if ( qw(k) > 0d0) then
                 uqcx(k+1) = uqcx(k+1) - qw(k)*ucx(k)*rho(k)
                 uqcx(k  ) = uqcx(k  ) + qw(k)*ucx(k)*rho(k)
                 uqcy(k+1) = uqcy(k+1) - qw(k)*ucy(k)*rho(k)
                 uqcy(k  ) = uqcy(k  ) + qw(k)*ucy(k)*rho(k)
             else if ( qw(k) < 0d0) then
                 uqcx(k+1) = uqcx(k+1) - qw(k)*ucx(k+1)*rho(k+1)
                 uqcx(k  ) = uqcx(k  ) + qw(k)*ucx(k+1)*rho(k+1)
                 uqcy(k+1) = uqcy(k+1) - qw(k)*ucy(k+1)*rho(k+1)
                 uqcy(k  ) = uqcy(k  ) + qw(k)*ucy(k+1)*rho(k+1)
             endif
             sqa(k+1) = sqa(k+1) - qw(k)
             sqa(k  ) = sqa(k  ) + qw(k)
          enddo
       enddo
    endif
 endif

 if (jarhoxu > 0) then
    sqa = sqa*rho
 endif

 do n  = 1,numsrc                             ! momentum
    if (arsrc(n) > 0) then                    ! if momentum desired
       if (qsrc(n) > 0) then
          kk  = ksrc(4,n)                     ! 2D pressure cell nr TO
          ksb = ksrc(5,n)                     ! cell nr
          kst = ksrc(6,n)                     ! cell nr
       else
          kk  = ksrc(1,n)                     ! 2D pressure cell nr FROM
          ksb = ksrc(2,n)                     ! cell nr
          kst = ksrc(3,n)                     ! cell nr
       endif

       if (kk > 0 .and. ksb > 0) then

          qnn = qsrc(n)
          do k  = ksb,kst
             qn = qnn
             if (kmx > 0) then
                dzss  = zws(kst) - zws(ksb-1)
                if (dzss > epshs) then
                   qn = qnn*( zws(k) - zws(k-1) ) / dzss
                else
                   qn = qnn / (kst - ksb + 1)
                endif
             endif
             uqn = qn*qnn / arsrc(n)

             if (jarhoxu > 0) then
                rhoinsrc = rhomean   ! just for now
                qn       = qn* rhoinsrc
                uqn      = uqn*rhoinsrc
             endif

             if (qsrc(n) > 0) then               ! from 1 to 2
                uqcx(k) = uqcx(k) - uqn*cssrc(2,n)
                uqcy(k) = uqcy(k) - uqn*snsrc(2,n)
                sqa(k)  = sqa(k)  - qn           ! sqa : out - in
             else                                ! from 2 to 1
                uqcx(k) = uqcx(k) + uqn*cssrc(1,n)
                uqcy(k) = uqcy(k) + uqn*snsrc(1,n)
                sqa(k)  = sqa(k)  + qn           ! sqa : out - in
             endif

          enddo

       endif
    endif
 enddo

 nfw = 0

 if (kmx == 0) then

 !$OMP PARALLEL DO                                                                   &
 !$OMP PRIVATE(L, advel,k1,k2,iadvL,qu1,qu2,volu,ai,ae,iad,volui,abh,hh,v12t,ku,kd,isg,n12, ucxku, ucyku, ucin, fdx)

 do L  = 1,lnx

  advel = 0                                          !  advi (1/s), adve (m/s2)

  if ( hu(L) > 0 ) then

    k1    = ln(1,L) ; k2 = ln(2,L)
    iadvL = iadv(L)

    if (L > lnxi) then
       if (iadvL == 77) then
          if (u0(L) < 0) iadvL = 0
       else if (u0(L) > 0) then
           iadvL = 0                                 ! switch off advection for inflowing waterlevel bnd's, if not normalvelocitybnds
       endif
       !vol1(k1) = 0d0
    endif

    if (iadvL == 33) then                       !

       if (jasfer3d == 1) then
          qu1   = csu(L)*nod2linx(L,1,uqcx(k1),uqcy(k1)) + snu(L)*nod2liny(L,1,uqcx(k1),uqcy(k1)) - u1(L)*sqa(k1)
          qu2   = csu(L)*nod2linx(L,2,uqcx(k2),uqcy(k2)) + snu(L)*nod2liny(L,2,uqcx(k2),uqcy(k2)) - u1(L)*sqa(k2)
       else
          qu1   = csu(L)*uqcx(k1) + snu(L)*uqcy(k1) - u1(L)*sqa(k1)
          qu2   = csu(L)*uqcx(k2) + snu(L)*uqcy(k2) - u1(L)*sqa(k2)
       endif

       if (jarhoxu == 0) then
          if (kcu(L) == 1) then
             volu  = acl(L)*vol1_f(k1) + (1d0-acl(L))*vol1_f(k2)
          else
             volu  = acl(L)*vol1(k1) + (1d0-acl(L))*vol1(k2)
          endif
       else
          if (kcu(L) == 1) then
             volu  = acl(L)*vol1_f(k1)*rho(k1) + (1d0-acl(L))*vol1_f(k2)*rho(k2)
          else
             volu  = acl(L)*vol1(k1)*rho(k1) + (1d0-acl(L))*vol1(k2)*rho(k2)
          endif
       endif

       if (volu > 0) then
          advel = (acl(L)*qu1 + (1d0-acl(L))*qu2) / volu

          !if ( japiaczek33 == 1) then
          !   expl = ( acl(L)*sqa(k1) + (1d0-acl(L))*sqa(k2) ) / volu
          !   if (expl < 0d0) then
          !      advel   = advel   + expl*u1(L)
          !      advi(L) = advi(L) - expl
          !   endif
          !endif

       endif

    else if (iadvL == 44) then                       !

       if (vol1(k1) > 0) then
          if (jasfer3D == 1) then
             qu1   = csu(L)*nod2linx(L,1,uqcx(k1),uqcy(k1)) + snu(L)*nod2liny(L,1,uqcx(k1),uqcy(k1)) - u1(L)*sqa(k1)
          else
             qu1   = csu(L)*uqcx(k1) + snu(L)*uqcy(k1) - u1(L)*sqa(k1)
          endif
          advel = advel + acl(L)*qu1/vol1(k1)
       endif
       if (vol1(k2) > 0) then
          if (jasfer3D == 1) then
             qu2   = csu(L)*nod2linx(L,2,uqcx(k2),uqcy(k2)) + snu(L)*nod2liny(L,2,uqcx(k2),uqcy(k2)) - u1(L)*sqa(k2)
          else
             qu2   = csu(L)*uqcx(k2) + snu(L)*uqcy(k2) - u1(L)*sqa(k2)
          endif
          advel = advel +  (1d0-acl(L))*qu2 / vol1(k2)
       endif

    else if (iadvL == 3) then                             ! explicit first order mom conservative
                                                     ! based upon cell center excess advection velocity
       qu1 = 0                                       ! and Perot control volume
       if (vol1(k1) > 0) then
          qu1 = QucPer(1,L)                          ! excess momentum in/out u(L) dir. from k1
          qu1 = qu1*acl(L)                           ! Perot weigthing
       endif
       qu2 = 0
       if (vol1(k2) > 0) then
          qu2 = QucPer(2,L)                          ! excess momentum in/out u(L) dir. from k2
          qu2 = qu2*(1d0-acl(L))                     ! Perot weigthing
       endif
       volu  = acl(L)*vol1(k1) + (1d0-acl(L))*vol1(k2)
       if (volu > 0) then
          advel = (qu1 + qu2)/volu                   ! dimension: ((m4/s2) / m3) =   (m/s2)
       endif

   else if (iadvL == 103) then                       ! explicit first order mom conservative
                                                     ! based upon cell center excess advection velocity
       qu1 = 0                                       ! and Perot control volume
       qu2 = 0
       if (jaPure1D == 1) then 
   
          if (vol1_f(k1) > 0) then
             qu1 = QucPerPure1D(1,L)                    ! excess momentum in/out u(L) dir. from k1
             qu1 = qu1*acl(L)                           ! Perot weigthing
          endif
          if (vol1_f(k2) > 0) then
             qu2 = QucPerPure1D(2,L)                    ! excess momentum in/out u(L) dir. from k2
             qu2 = qu2*(1d0-acl(L))                     ! Perot weigthing
          endif
          volu = acl(L)*vol1_f(k1) + (1d0-acl(L))*vol1_f(k2)
          if (volu > 0) then
             advel = (qu1 + qu2)/volu                   ! dimension: ((m4/s2) / m3) =   (m/s2)
          endif

       else 
  
          if (vol1(k1) > 0) then
             qu1 = QucPerPure1D(1,L)                    ! excess momentum in/out u(L) dir. from k1
             qu1 = qu1*acl(L)                           ! Perot weigthing
          endif
          if (vol1(k2) > 0) then
             qu2 = QucPerPure1D(2,L)                    ! excess momentum in/out u(L) dir. from k2
             qu2 = qu2*(1d0-acl(L))                     ! Perot weigthing
          endif
          volu = acl(L)*vol1(k1) + (1d0-acl(L))*vol1(k2)
          if (volu > 0) then
             advel = (qu1 + qu2)/volu                   ! dimension: ((m4/s2) / m3) =   (m/s2)
          endif

       endif

    else if (iadvL == 333) then                      ! explicit first order mom conservative
                                                     ! based upon cell center excess advection velocity
       qu1 = 0                                       ! and Perot control volume
       if (volau(k1) > 0) then
          qu1 = QucPer(1,L)                          ! excess momentum in/out u(L) dir. from k1
          qu1 = qu1*acl(L)/ volau(k1)                ! Perot weigthing
       endif
       qu2 = 0
       if (volau(k2) > 0) then
          qu2 = QucPer(2,L)                          ! excess momentum in/out u(L) dir. from k2
          qu2 = qu2*(1d0-acl(L))/ volau(k2)                     ! Perot weigthing
       endif
       advel = qu1 + qu2                             ! dimension: ((m4/s2) / m3) =   (m/s2)

    else if (iadvL == 30) then                       ! Same as 3, now with alfa = 0.5 in volumes and advection
                                                     ! based upon cell center excess advection velocity
       qu1 = 0
       if (vol1(k1) > 0) then
          qu1 = QucPer(1,L)                          ! excess momentum in/out u(L) dir. from k1
       endif
       qu2 = 0
       if (vol1(k2) > 0) then
          qu2 = QucPer(2,L)                          ! excess momentum in/out u(L) dir. from k2
       endif
       volu  = vol1(k1) + vol1(k2)
       if (volu > 0) then
          advel = (qu1 + qu2)/volu                   ! dimension: ((m4/s2) / m3) =   (m/s2)
       endif

    else if (iadvL == 31) then                       ! Thesis Olga 4.8
                                                     ! based upon cell center excess advection velocity
       if (jasfer3D == 1) then
          qu1   = csu(L)*nod2linx(L,1,uqcx(k1),uqcy(k1)) + snu(L)*nod2liny(L,1,uqcx(k1),uqcy(k1))
          qu2   = csu(L)*nod2linx(L,2,uqcx(k2),uqcy(k2)) + snu(L)*nod2liny(L,2,uqcx(k2),uqcy(k2))
       else
          qu1   = csu(L)*uqcx(k1) + snu(L)*uqcy(k1)
          qu2   = csu(L)*uqcx(k2) + snu(L)*uqcy(k2)
       endif
       advel = acl(L)*qu1 + (1d0-acl(L))*qu2

    else if (iadvL == 40) then                       !

       if (jasfer3D == 1) then
          qu1   = csu(L)*nod2linx(L,1,uqcx(k1),uqcy(k1)) + snu(L)*nod2liny(L,1,uqcx(k1),uqcy(k1)) - u1(L)*sqa(k1)
          qu2   = csu(L)*nod2linx(L,2,uqcx(k2),uqcy(k2)) + snu(L)*nod2liny(L,2,uqcx(k2),uqcy(k2)) - u1(L)*sqa(k2)
       else
          qu1   = csu(L)*uqcx(k1) + snu(L)*uqcy(k1) - u1(L)*sqa(k1)
          qu2   = csu(L)*uqcx(k2) + snu(L)*uqcy(k2) - u1(L)*sqa(k2)
       endif
       volu  = acl(L)*voldhu(k1) + (1d0-acl(L))*voldhu(k2)

       if (volu > 0) then
          advel = (acl(L)*qu1 + (1d0-acl(L))*qu2) / volu
       endif

    else if (iadvL == 1) then                        ! explicit first order mom conservative
                                                     ! based upon cell center advection velocity
                                                     ! and Wenneker control volume, now with
                                                     ! uqcx and uqcy arrays instead of function call, (much faster than excess form)

        volu      = vol1(k1) + vol1(k2)              ! Wennekers control volume
                                                     ! qu1     = ( uqcx(k1)*cs + uqcy(k1)*sn )
                                                     ! qu2     = ( uqcx(k2)*cs + uqcy(k2)*sn )
       if (volu   > 0) then
          if (jasfer3D == 1) then
             qu1 = csu(L)*(nod2linx(L,1,uqcx(k1),uqcy(k1)) + nod2linx(L,2,uqcx(k2),uqcy(k2)))
             qu2 = snu(L)*(nod2liny(L,1,uqcx(k1),uqcy(k1)) + nod2liny(L,2,uqcx(k2),uqcy(k2)))
          else
             qu1 = csu(L)*( uqcx(k1) + uqcx(k2) )
             qu2 = snu(L)*( uqcy(k1) + uqcy(k2) )
          endif
          v12t    = sq(k1) + sq(k2)                  ! time der. of v12
          advel   = (qu1 + qu2 + u1(L)*v12t) / volu  ! dimension: ((m4/s2) / m3) =   (m/s2)
       endif

    else if (iadvL == 2) then                        ! explicit first order mom conservative
                                                     ! based upon cell center excess advection velocity
       volu     = vol1(k1) + vol1(k2)                ! Wennekers control volume
       if (volu > 0) then
          qu1   = QucWen(1,L)                        ! excess momentum in u(L) dir. out of k1
          qu2   = QucWen(2,L)                        ! out of k2
          advel = (qu1 + qu2) / volu                 ! dimension: ((m4/s2) / m3) =   (m/s2)
       endif

   else if (iadvL == 4) then                         ! explicit first order mom conservative

       qu1 = 0                                       ! and Perot control volume
       if (vol1(k1) > 0) then
          qu1 = QucPeri(1,L)                         ! excess momentum in u(L) dir. from of k1
          qu1 = qu1*acl(L)                           ! Perot weigthing
       endif
       qu2 = 0
       if (vol1(k2) > 0) then
          qu2 = QucPeri(2,L)                         ! excess momentum in u(L) dir. from of k2
          qu2 = qu2*(1d0-acl(L))                     ! Perot weigthing
       endif

       volu  = acl(L)*vol1(k1) + (1d0-acl(L))*vol1(k2)
       if (volu > 0) then
          advel = (qu1 + qu2)/volu                   ! dimension: ((m4/s2) / m3) =   (m/s2)
       endif

    else if (iadvL == 5 .or. iadvL ==6) then         ! 5,6 = advection like 3,4, now Piaczek teta

       if (kcu(L) ==1) then
          volu = acl(L)*vol1_f(k1) + (1d0-acl(L))*vol1_f(k2)
       else
          volu = acl(L)*vol1(k1) + (1d0-acl(L))*vol1(k2)
       endif

       if (volu > 0) then
          volui = 1d0/volu
          if (vol1(k1) > 0) then
             call QucPeripiaczekteta(1,L,ai,ae,volu,iadvL-2)   ! excess momentum in u(L) dir. out of k1, include own
             abh     = acl(L)*volui
             adveL   = adveL   + abh*ae
             advi(L) = advi(L) + abh*ai
          endif
          if (vol1(k2) > 0) then
             call QucPeripiaczekteta(2,L,ai,ae,volu,iadvL-2)   ! excess momentum in u(L) dir. out of k2
             abh = (1d0-acl(L))*volui
             adveL   = adveL   + abh*ae
             advi(L) = advi(L) + abh*ai
          endif

       endif

    else if (iadvL >= 7 .and. iadvL <= 12) then      ! Piaczek fully implicit

       iad  = 3
       if (iadvL == 8 .or. iadvL == 10 .or. iadvL == 12) then
          iad = 4
       endif

       if (kcu(L) ==1) then
          volu = acl(L)*vol1_f(k1) + (1d0-acl(L))*vol1_f(k2)
       else if (kcu(L) == 3 .and. iadveccorr1D2D == 1) then
          volu = au(L)*dx(L) ! Use volume weighting based on approximated "lateral volume", to avoid large 1D river volumes.
       else
          volu = acl(L)*vol1(k1) + (1d0-acl(L))*vol1(k2)
       endif

       if (volu > 0) then
          volui = 1d0/volu
          if (hs(k1) > 0) then
             call QucPeripiaczek(1,L,ai,ae,iad)   ! excess momentum in u(L) dir. out of k1, include own
             abh     = acl(L)*volui
             adveL   = adveL   + abh*ae
             advi(L) = advi(L) + abh*ai
          endif
          if (hs(k2) > 0) then
             call QucPeripiaczek(2,L,ai,ae,iad)   ! excess momentum in u(L) dir. out of k2
             abh = (1d0-acl(L))*volui
             adveL   = adveL   + abh*ae
             advi(L) = advi(L) + abh*ai
          endif

       endif

    else if (iadvL == 21) then                        ! subgrid weir small stencil, ifixedweirscheme = 3
                                                      ! upwind center velocity does not feel crest link

       !advel = 0.5d0*(  u0(L)*u0(L) - u0(L-1)*u0(L-1) ) / dx(L)

       if (u0(L)  > 0d0) then
          ku = k1 ; kd = k2 ; isg =  1 ; n12 = 1
       else
          ku = k2 ; kd = k1 ; isg = -1 ; n12 = 2
       endif

       call getucxucynoweirs(ku, ucxku, ucyku, ifixedweirscheme )
       if (jasfer3D == 1) then
          ucin = nod2linx(L,n12,ucxku,ucyku)*csu(L) + nod2liny(L,n12,ucxku,ucyku)*snu(L)
       else
          ucin = ucxku*csu(L) + ucyku*snu(L)
       endif

       fdx     = 0.5d0*dxi(L)*isg

       advi(L) = advi(L) + fdx*u0(L)
       advel   = advel   - fdx*ucin*ucin

    !    advel   = fdx*(u0(L)*u0(L) - ucin*ucin)

    else if (iadvL == 77) then                        ! supercritical inflow boundary

       abh     = bai(k1)*huvli(L)*acl(L)
       if (jasfer3D == 1) then
          adveL = adveL   - abh*q1(L)*(nod2linx(L,1,ucx(k1),ucy(k1))*csu(L) + nod2liny(L,1,ucx(k1),ucy(k1))*snu(L))
       else
          adveL = adveL   - abh*q1(L)*(ucx(k1)*csu(L)+ucy(k1)*snu(L))
       endif
       advi(L) = advi(L) + abh*q1(L)

    else if (iadvL == 38) then                       ! explicit first order mom conservative olga (17)
                                                     ! based upon cell center excess advection velocity
       qu1 = 0                                       ! and Perot control volume
       if (vol1(k1) > 0) then
          qu1 = QucPercu(1,L)                        ! excess momentum in/out uc(k1) dir. from k1
          qu1 = qu1*acl(L)/volau(k1)                 ! Perot weigthing
       endif
       qu2 = 0
       if (vol1(k2) > 0) then
          qu2 = QucPercu(2,L)                        ! excess momentum in/out uc(k2) dir. from k2
          qu2 = qu2*(1d0-acl(L))/volau(k2)           ! Perot weigthing
       endif
       advel = qu1 + qu2                             ! dimension: ((m4/s2) / m3) =   (m/s2)

    else if (iadvL == 34) then                       ! explicit first order mom conservative (stelling kramer)
                                                     ! based upon cell center excess advection velocity
       qu1 = 0                                       ! and Perot control volume
       if (vol1(k1) > 0) then
          qu1 = QucPer(1,L)                          ! excess momentum in/out u(L) dir. from k1
          qu1 = qu1*acl(L)*bai(k1)                   ! Perot weigthing
       endif
       qu2 = 0
       if (vol1(k2) > 0) then
          qu2 = QucPer(2,L)                          ! excess momentum in/out u(L) dir. from k2
          qu2 = qu2*(1d0-acl(L))*bai(k2)             ! Perot weigthing
       endif
       advel = (qu1 + qu2)*huvli(L)                  ! dimension: ((m4/s2) / m3) =   (m/s2)

    else if (iadvL == 35) then                       ! explicit first order mom conservative (stelling kramer)
                                                     ! based upon cell center excess advection velocity
       qu1 = 0                                       ! and Perot control volume
       if (vol1(k1) > 0) then
          qu1 = QufPer(1,L)                          ! excess momentum in/out u(L) dir. from k1
          qu1 = qu1*acl(L)                           ! Perot weigthing
       endif
       qu2 = 0
       if (vol1(k2) > 0) then
          qu2 = QufPer(2,L)                          ! excess momentum in/out u(L) dir. from k2
          qu2 = qu2*(1d0-acl(L))                     ! Perot weigthing
       endif
       volu  = acl(L)*vol1(k1) + (1d0-acl(L))*vol1(k2)
       if (volu > 0) then
          advel = (qu1 + qu2)/volu                   ! dimension: ((m4/s2) / m3) =   (m/s2)
       endif

    else if (iadvL == 36) then                       ! explicit first order mom conservative
                                                     ! based upon cell center excess advection velocity
       qu1 = 0                                       ! and Perot control volume
       if (vol1(k1) > 0) then
          qu1 = QucPerq1(1,L)                        ! excess momentum in/out uc(k1) dir. from k1
          qu1 = qu1*acl(L)/vol1(k1)                  ! Perot weigthing
       endif
       qu2 = 0
       if (vol1(k2) > 0) then
          qu2 = QucPerq1(2,L)                        ! excess momentum in/out uc(k2) dir. from k2
          qu2 = qu2*(1d0-acl(L))/vol1(k2)            ! Perot weigthing
       endif
       advel = qu1 + qu2                             ! dimension: ((m4/s2) / m3) =   (m/s2)

    else if (iadvL == 37) then                       ! Kramer Stelling
       qu1 = 0d0
       if (vol1(k1) > 0) then
          qu1 = acl(L)*QucPerq1(1,L)/ba(k1)          ! excess momentum in/out u(L) dir. from k1
       endif
       qu2 = 0d0
       if (vol1(k2) > 0) then
          qu2 = (1d0-acl(L))*QucPerq1(2,L)/ba(k2)    ! excess momentum in/out u(L) dir. from k1
       endif
       advel = huvli(L)*(qu1 + qu2)

    endif

    adve(L) = adve(L) + advel

  endif

 enddo

 !$OMP END PARALLEL DO

 else                                                      ! Plus vertical

 do LL  = 1,lnx

    if ( hu(LL) > 0 ) then

       iadvL = iadv(LL)
       if (LL > lnxi) then
          if (iadvL == 77) then
             if (u0(LL) < 0) cycle
          else if (u0(LL) > 0) then
             cycle                                            ! switch off advection for inflowing waterlevel bnd's, if not normalvelocitybnds
          endif
       endif
       cs  = csu(LL)  ; sn  = snu(LL)
       Lb  = Lbot(LL) ; Lt  = Ltop(LL)
       ac1 = acl(LL)  ; ac2 = 1d0 - ac1

       if (iadv(LL) == 3) then
          call QucPer3Dsigma(1,LL,Lb,Lt,cs,sn,quk1)           ! sum of (Q*uc cell centre upwind normal) at side 1 of basis link LL
          call QucPer3Dsigma(2,LL,Lb,Lt,cs,sn,quk2)           ! sum of (Q*uc cell centre upwind normal) at side 2 of basis link LL

          do L = Lb, Lt
             advel = 0d0                                      ! advi (1/s), adve (m/s2)
             k1    = ln(1,L) ; k2 = ln(2,L)
             qu1   = 0d0
             if (vol1(k1) > 0) then
                qu1 = quk1(1,L-Lb+1)*ac1                      ! Perot weigthing
             endif
             qu2    = 0d0
             if (vol1(k2) > 0) then
                qu2 = quk2(1,L-Lb+1)*ac2                      ! Perot weigthing
             endif
             if (jarhoxu > 0) then
                volu  = ac1*vol1(k1)*rho(k1) + ac2*vol1(k2)*rho(k2)
             else
                volu  = ac1*vol1(k1)         + ac2*vol1(k2)
             endif
             if (volu > 0) then
                advel = (qu1 + qu2)/volu                      ! dimension: ((m4/s2) / m3) =   (m/s2)
             endif
             adve(L) = adve(L) + advel

          enddo

       else if ( iadv(LL) == 33 .or. iadv(LL) == 40 .or. iadv(LL) == 6 ) then                       !

          ! qu1   = csu(L)*uqcx(k1) + snu(L)*uqcy(k1) - u1(L)*sqa(k1)
          ! qu2   = csu(L)*uqcx(k2) + snu(L)*uqcy(k2) - u1(L)*sqa(k2)
          ! volu  = ac1*vol1(k1)    + ac2*vol1(k2)
          ! if (volu > 0) then
          !    advel = (acl(L)*qu1 + (1d0-acl(L))*qu2) / volu
          ! endif

          if (layertype == 1) then

             if (iadv(LL) == -6 ) then  ! .and. newzbndadv == 1 ) then

                do L = Lb, Lt
                   if (u1(L) > 0) then
                      k = ln(1,L)
                      n12 = 1
                   else
                      k = ln(2,L)
                      n12 = 2
                   endif
                   if (jasfer3D == 1) then
                      advel   = 2d0*( u1(L) - (cs*nod2linx(LL,n12,ucx(k),ucy(k)) + sn*nod2liny(LL,n12,ucx(k),ucy(k)) ) )*dxi(LL)
                   else
                      advel   = 2d0*( u1(L) - (cs*ucx(k) + sn*ucy(k) ) )*dxi(LL)
                   endif
                   if ( advel > 0d0 ) then
                      advi(L) = advi(L) + advel
                   else
                      adve(L) = adve(L) - cs*u1(L)*advel
                   endif
                enddo

             else

             do L = Lb, Lt
                k1    = ln(1,L) ; k2 = ln(2,L)
                if (jasfer3D == 1) then
                   qu1   =  cs*nod2linx(LL,1,uqcx(k1),uqcy(k1)) +  sn*nod2liny(LL,1,uqcx(k1),uqcy(k1)) - u1(L)*sqa(k1)
                   qu2   =  cs*nod2linx(LL,2,uqcx(k2),uqcy(k2)) +  sn*nod2liny(LL,2,uqcx(k2),uqcy(k2)) - u1(L)*sqa(k2)
                else
                   qu1   =  cs*uqcx(k1) + sn*uqcy(k1) - u1(L)*sqa(k1)
                   qu2   =  cs*uqcx(k2) + sn*uqcy(k2) - u1(L)*sqa(k2)
                endif

                if (jarhoxu > 0) then
                   !if (kcu(LL) ==1) then
                   !   volu  = ac1*vol1_f(k1)*rho(k1) + ac2*vol1_f(k2)*rho(k2)
                   !else
                      volu  = ac1*vol1(k1)*rho(k1) + ac2*vol1(k2)*rho(k2)
                   !endif
                else
                   !if (kcu(LL) ==1) then
                   !   volu  = ac1*vol1_f(k1)         + ac2*vol1_f(k2)
                   !else
                      volu  = ac1*vol1(k1)         + ac2*vol1(k2)
                   !endif
                endif

                if (volu > 0) then

                   adve(L) = adve(L) + (ac1*qu1 + ac2*qu2) / volu

                   !if ( japiaczek33 == 1) then
                   !   expl = ( acl(L)*sqa(k1) + (1d0-acl(L))*sqa(k2) ) / volu
                   !   if (expl < 0d0) then
                   !      adve(L) = adve(L) + expl*u1(L)
                   !      advi(L) = advi(L) - expl
                   !   endif
                   !endif

                endif
             enddo

             endif

          else if (layertype == 2 .and. jahazlayer == 0) then ! default fixed layers

             Ltx = Lt-Lb+1
             volukk(1:Ltx) = 0d0
             do L = Lb, Lt
                k1    = ln(1,L) ; k2 = ln(2,L)
                if (jarhoxu > 0) then
                   volukk(L-Lb+1)  = volukk(L-Lb+1) + ac1*vol1(k1)*rho(k1) + ac2*vol1(k2)*rho(k2)
                else
                   volukk(L-Lb+1)  = volukk(L-Lb+1) + ac1*vol1(k1) + ac2*vol1(k2)
                endif
             enddo
             do k = k1+1, ktop(ln(1,LL) )
                if (jarhoxu > 0) then
                   volukk(Lt-Lb+1) = volukk(Lt-Lb+1) + ac1*vol1(k)*rho(k)
                else
                   volukk(Lt-Lb+1) = volukk(Lt-Lb+1) + ac1*vol1(k)
                endif
             enddo
             do k = k2+1, ktop(ln(2,LL) )
                if (jarhoxu > 0) then
                   volukk(Lt-Lb+1) = volukk(Lt-Lb+1) + ac2*vol1(k)*rho(k)
                else
                   volukk(Lt-Lb+1) = volukk(Lt-Lb+1) + ac2*vol1(k)
                endif
             enddo

             do L = Lb, Lt
                k1    = ln(1,L) ; k2 = ln(2,L)
                if (jasfer3D == 1) then
                   qu1    = cs*nod2linx(LL,1,uqcx(k1),uqcy(k1)) + sn*nod2liny(LL,1,uqcx(k1),uqcy(k1)) - u1(L)*sqa(k1)
                   qu2    = cs*nod2linx(LL,2,uqcx(k2),uqcy(k2)) + sn*nod2liny(LL,2,uqcx(k2),uqcy(k2)) - u1(L)*sqa(k2)
                else
                   qu1    = cs*uqcx(k1) + sn*uqcy(k1) - u1(L)*sqa(k1)
                   qu2    = cs*uqcx(k2) + sn*uqcy(k2) - u1(L)*sqa(k2)
                endif

                if (volukk(L-Lb+1) > 0) then
                   adve(L) = adve(L) +  (ac1*qu1 + ac2*qu2) / volukk(L-Lb+1)

                   !if ( japiaczek33 == 1) then
                   !   expl = ( acl(L)*sqa(k1) + (1d0-acl(L))*sqa(k2) ) / volukk(L-Lb+1)
                   !   if (expl < 0d0) then
                   !      adve(L) = adve(L) + expl*u1(L)
                   !      advi(L) = advi(L) - expl
                   !   endif
                   !endif

                endif

             enddo

          else if (layertype == 2 .and. jahazlayer == 1) then

             n1  = ln(1,LL) ; n2 = ln(2,LL)
             call getkbotktop(n1,kb1,kt1) ; ktx1 = kt1-kb1+1
             call getkbotktop(n2,kb2,kt2) ; ktx2 = kt2-kb2+1
             Ltx = Lt-Lb+1

             volukk(1:Ltx) = 0d0

             do L = Lb, Lt
                k1    = ln(1,L) ; k2 = ln(2,L) ; L1 = L-Lb+1
                volukk(L1) = volukk(L1) + ac1*vol1(k1) + ac2*vol1(k2)
             enddo

             do k = k1+1, kt1
                volukk(Ltx) = volukk(Ltx) + ac1*vol1(k)
             enddo

             do k = k2+1, kt2
                volukk(Ltx) = volukk(Ltx) + ac2*vol1(k)
             enddo

             do L = Lb, Lt
                k1    = ln(1,L) ; k2 = ln(2,L) ; L1 = L-Lb+1
                if (volukk(L1) > 0) then
                   if (jasfer3D == 1) then
                      qu1   =  cs*nod2linx(LL,1,uqcx(k1),uqcy(k1)) +  sn*nod2liny(LL,1,uqcx(k1),uqcy(k1)) - u1(L)*sqa(k1)
                      qu2   =  cs*nod2linx(LL,2,uqcx(k2),uqcy(k2)) +  sn*nod2liny(LL,2,uqcx(k2),uqcy(k2)) - u1(L)*sqa(k2)
                   else
                      qu1     = cs*uqcx(k1) + sn*uqcy(k1) - u1(L)*sqa(k1)
                      qu2     = cs*uqcx(k2) + sn*uqcy(k2) - u1(L)*sqa(k2)

                   endif
                   adve(L) = adve(L) + (ac1*qu1 + ac2*qu2) / volukk(L-Lb+1)
                endif
             enddo

          else if (layertype == 2 .and. jahazlayer == 2 ) then  ! lineinterp

             n1  = ln(1,LL) ; n2 = ln(2,LL)
             call getkbotktop(n1,kb1,kt1)
             call getkbotktop(n2,kb2,kt2)
             hs1 = max(epshs, zws(kt1) - zws(kb1-1) )
             hs2 = max(epshs, zws(kt2) - zws(kb2-1) )

             ktx01 = kt1 - kb1 + 1
             ktx02 = kt2 - kb2 + 1

             volk1(0) = 0d0 ; quuk1(0) = 0d0 ; sqak1(0) = 0d0 ; sigk1(0) = 0d0
             do k = kb1, kt1
                volk1(k-kb1+1) = volk1(k-kb1) + vol1(k)
                if (jasfer3D == 1) then
                   quuk1(k-kb1+1) = quuk1(k-kb1) + cs*nod2linx(LL,1,uqcx(k),uqcy(k)) + sn*nod2liny(LL,1,uqcx(k),uqcy(k))
                else
                   quuk1(k-kb1+1) = quuk1(k-kb1) + cs*uqcx(k) + sn*uqcy(k)
                endif
                sqak1(k-kb1+1) = sqak1(k-kb1) + sqa(k)
                sigk1(k-kb1+1) = ( zws(k) - zws(kb1-1) ) / hs1
             enddo

             volk2(0) = 0d0 ; quuk2(0) = 0d0 ; sqak2(0) = 0d0 ; sigk2(0) = 0d0
             do k = kb2, kt2
                volk2(k-kb2+1) = volk2(k-kb2) + vol1(k)
                if (jasfer3D == 1) then
                   quuk2(k-kb2+1) = quuk2(k-kb2) + cs*nod2linx(LL,2,uqcx(k),uqcy(k)) + sn*nod2liny(LL,2,uqcx(k),uqcy(k))
                else
                   quuk2(k-kb2+1) = quuk2(k-kb2) + cs*uqcx(k) + sn*uqcy(k)
                endif
                sqak2(k-kb2+1) = sqak2(k-kb2) + sqa(k)
                sigk2(k-kb2+1) = ( zws(k) - zws(kb2-1) ) / hs2
             enddo

             do L = Lb, Lt  ;  Ltx0 = Lt - Lb + 1 ; siguL(0) = 0d0
                siguL(L-Lb+1) = hu(L) / hu(LL)
             enddo

             call lineinterp3( siguL, quuL1, volL1, sqaL1, Ltx0, sigk1, quuk1, volk1, sqak1, ktx01)
             call lineinterp3( siguL, quuL2, volL2, sqaL2, Ltx0, sigk2, quuk2, volk2, sqak2, ktx02)

             do L = Lb, Lt
                vo1  = volL1(L-Lb+1) - volL1(L-Lb)
                vo2  = volL2(L-Lb+1) - volL2(L-Lb)
                volu = vo1*ac1 + vo2*ac2

                if (volu > 0) then
                   qu1   = quuL1(L-Lb+1) - quuL1(L-Lb) - u1(L)*( sqaL1(L-Lb+1) - sqaL1(L-Lb) )
                   qu2   = quuL2(L-Lb+1) - quuL2(L-Lb) - u1(L)*( sqaL2(L-Lb+1) - sqaL2(L-Lb) )
                   advel = ( ac1*qu1 + ac2*qu2 ) / volu
                   adve(L) = adve(L) + advel
                endif
             enddo

          else if (layertype == 2 .and. jahazlayer == 4) then

             n1  = ln(1,LL) ; n2 = ln(2,LL)
             call getkbotktop(n1,kb1,kt1) ; ktx1 = kb1 + kmxn(n1) - 1
             call getkbotktop(n2,kb2,kt2) ; ktx2 = kb2 + kmxn(n2) - 1

             Ltx = Lt-Lb+1

             volukk(1:Ltx) = 0d0 ; quuk1(1:Ltx) = 0d0 ; sqak1(1:Ltx) = 0d0

             do k = kb1, ln(1,Lb) - 1                   ! below Lb n1
                volukk(1) = volukk(1)     + ac1*vol1(k)
                if (jasfer3D == 1) then
                   quuk1(1) = quuk1(1) + ac1*(cs*nod2linx(LL,1,uqcx(k),uqcy(k)) + sn*nod2liny(LL,1,uqcx(k),uqcy(k)))
                else
                   quuk1(1) = quuk1 (1)     + ac1*(cs*uqcx(k) + sn*uqcy(k))
                endif
                sqak1 (1) = sqak1 (1)     + ac1*sqa(k)
             enddo

             do k = kb2, ln(2,Lb) - 1                   ! below Lb n2
                volukk(1) = volukk(1)     + ac2*vol1(k)
                if (jasfer3D == 1) then
                   quuk1(1) = quuk1(1) + ac2*(cs*nod2linx(LL,2,uqcx(k),uqcy(k)) + sn*nod2liny(LL,2,uqcx(k),uqcy(k)))
                else
                   quuk1(1) = quuk1 (1)     + ac2*(cs*uqcx(k) + sn*uqcy(k))
                endif
                sqak1 (1) = sqak1 (1)     + ac2*sqa(k)
             enddo

             do L = Lb, Lt                              ! intermediate
                k1    = ln(1,L) ; k2 = ln(2,L) ; L1 = L-Lb+1
                volukk(L1) = volukk(L1) + ac1*vol1(k1)                    + ac2*vol1(k2)
                if (jasfer3D == 1) then
                   quuk1 (L1) = quuk1(L1) + ac1*(cs*nod2linx(LL,1,uqcx(k1),uqcy(k1)) + sn*nod2liny(LL,1,uqcx(k1),uqcy(k1))) +   &
                                            ac2*(cs*nod2linx(LL,2,uqcx(k2),uqcy(k2)) + sn*nod2liny(LL,2,uqcx(k2),uqcy(k2)))
                else
                   quuk1 (L1) = quuk1 (L1) + ac1*(cs*uqcx(k1) + sn*uqcy(k1)) + ac2*(cs*uqcx(k2) + sn*uqcy(k2))
                endif
                sqak1 (L1) = sqak1 (L1) + ac1*sqa(k1)                     + ac2*sqa(k2)
             enddo

             do k = k1+1, ktx1                          ! above Lt n1
                volukk(Ltx) = volukk(Ltx) + ac1*vol1(k)
                if (jasfer3D == 1) then
                   quuk1 (Ltx) = quuk1(Ltx)  + ac1*(cs*nod2linx(LL,1,uqcx(k),uqcy(k)) + sn*nod2liny(LL,1,uqcx(k),uqcy(k)))
                else
                   quuk1 (Ltx) = quuk1(Ltx)  + ac1*(cs*uqcx(k) + sn*uqcy(k))
                endif
                sqak1 (Ltx) = sqak1(Ltx)  + ac1*sqa(k)
             enddo

             do k = k2+1, ktx2                          ! above Lt n2
                volukk(Ltx) = volukk(Ltx) + ac2*vol1(k)
                if (jasfer3D == 1) then
                   quuk1 (Ltx) = quuk1(Ltx)  + ac2*(cs*nod2linx(LL,2,uqcx(k),uqcy(k)) + sn*nod2liny(LL,2,uqcx(k),uqcy(k)))
                else
                   quuk1 (Ltx) = quuk1 (Ltx) + ac2*(cs*uqcx(k) + sn*uqcy(k))
                endif
                sqak1 (Ltx) = sqak1 (Ltx) + ac2*sqa(k)
             enddo

             do L  = Lb, Lt
                L1 = L-Lb+1
                if (volukk(L1) > 0) then
                   adveL   = ( quuk1(L1) - u1(L)*sqak1(L1) ) / volukk(L1)
                   if (abs(advel) > 0.05) then
                      advel = 1d0*advel
                   endif
                   adve(L) = adve(L) + adveL
               endif
             enddo

          endif

       else if (iadv(LL) == 34) then                          ! Kramer Stelling, ba per cell weighted

          call QucPer3Dsigma(1,LL,Lb,Lt,cs,sn,quk1)           ! sum of (Q*uc cell centre upwind normal) at side 1 of basis link LL
          call QucPer3Dsigma(2,LL,Lb,Lt,cs,sn,quk2)           ! sum of (Q*uc cell centre upwind normal) at side 2 of basis link LL
          baik1 = bai( ln(1,LL) )
          baik2 = bai( ln(2,LL) )
          do L = Lb, Lt
             advel = 0                                        ! advi (1/s), adve (m/s2)
             k1    = ln(1,L) ; k2 = ln(2,L)
             qu1   = 0d0
             if (vol1(k1) > 0) then
                qu1 = quk1(1,L-Lb+1)*ac1*baik1
             endif
             qu2    = 0
             if (vol1(k2) > 0) then
                qu2 = quk2(1,L-Lb+1)*ac2*baik2            ! Perot weigthing
             endif
             huvL    = ac1*(zws(k1)-zws(k1-1)) + ac2*(zws(k2)-zws(k2-1))
             if (huvL > 0d0) then
                advel   = (qu1 + qu2)/huvL                         ! dimension: ((m4/s2) / m3) =   (m/s2)
                adve(L) = adve(L) + advel
             endif
          enddo

       else if (iadv(LL) == 5) then

          call QucPer3Dsigmapiaczekteta(LL,Lb,Lt,cs,sn,quk1,quk2)

          do L = Lb, Lt
              adve(L) = adve(L) + quk1(1,L-Lb+1)
              advi(L) = advi(L) + quk2(1,L-Lb+1)
          enddo

       else if (iadv(LL) == 44) then

            do L = Lb, Lt
               k1    = ln(1,L) ; k2 = ln(2,L)
               if (vol1(k1) > 0) then
                  if (jasfer3D == 1) then
                     qu1     = cs*nod2linx(LL,1,uqcx(k1),uqcy(k1)) + sn*nod2liny(LL,1,uqcx(k1),uqcy(k1)) - u1(L)*sqa(k1)
                  else
                     qu1     = cs*uqcx(k1)  + sn*uqcy(k1) - u1(L)*sqa(k1)
                  endif
                  adve(L) = adve(L) + ac1*qu1/vol1(k1)
               endif
               if (vol1(k2) > 0) then
                  if (jasfer3D == 1) then
                     qu2    =  cs*nod2linx(LL,2,uqcx(k2),uqcy(k2)) + sn*nod2liny(LL,2,uqcx(k2),uqcy(k2)) - u1(L)*sqa(k2)
                  else
                     qu2     = cs*uqcx(k2)  + sn*uqcy(k2) - u1(L)*sqa(k2)
                  endif
                  adve(L) = adve(L) + ac2*qu2/vol1(k2)
               endif
            enddo

       endif   ! advectiontypes

    endif      ! (hu)

 enddo         ! LL

 endif

 if (kmx == 0 .and. lnx1D >0) then
    call setucxy1D()
 endif

 end subroutine advec
