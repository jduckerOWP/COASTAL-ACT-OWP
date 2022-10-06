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

! $Id: tekrai.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/tekrai.f90 $

 subroutine tekrai(nsiz,ja)

 use unstruc_colors
 use m_netw
 use m_flow
 use m_flowgeom
 use m_flowtimes
 use unstruc_model
 use unstruc_display
 use m_raaitek
 use m_missing
 use m_sediment
 use m_strucs
 use m_flowexternalforcings

 implicit none

 integer          :: nsiz, ja

 double precision :: xx1, xx2, zz
 integer          :: k1, k2, l1, l2, n1, n2
 double precision :: uu, ww, z1, z2
 double precision :: zfac, zgaten
 integer          :: l, k, kk, j, kplotorg, n, ncol

 double precision :: VMAX,VMIN,DV,VAL(256)
 integer          :: NCOLS(256),NIS,NIE,nv,JAAUTO
 double precision :: vfac, vfacforce, doorh
 integer          :: nvec, ng

 common /depmax/ vmax,vmin,dv,val,ncols,nv,nis,nie,jaauto
 COMMON /VFAC/ VFAC,VFACFORCE,NVEC
 common /drawthis/ ndraw(50)
 integer :: ndraw, kts

 double precision :: zz1, zz2, xz1, xz2
 double precision :: xmn, xmx, ymx, zmx, zmx2, bot, top, xx, yy, bup, xxu, zzu
 double precision :: xp(4), yp(4), zp(4), xxmn, xxmx, zn, dlay, dl, xp1, yp1, qsrck
 integer          :: mx, kb, kt, Lb, Lt, LL, kplotfrombedorsurfacesav
 double precision, external    :: znod, zlin

 double precision, allocatable ::   plotlin2(:)
 integer         , allocatable :: ip(:), ip2(:)
 logical inview


 if (ndx  < 1) return

 kplotfrombedorsurfacesav = kplotfrombedorsurface
 kplotfrombedorsurface    = 1

 if (nsiz > 3) then
    call poiseuille(0)
    return
 endif

 xmn = 1e10
 xmx = -xmn
 ymn = 1e10
 ymx = -ymn
 zmn = 1e4
 zmx = -zmn
 do k = 1,ndx
    xx = xz(k)
    yy = yz(k)
    if ( inview(xx,yy) ) then
       if (xz(k) < xmn ) xmn = xz(k)
       if (xz(k) > xmx ) xmx = xz(k)

       if (yz(k) < ymn ) ymn = yz(k)
       if (yz(k) > ymx ) ymx = yz(k)

       bot = bl(k)

       top = s1(k)

       zmn = min( zmn,min( bot, top ) )
       zmx = max( zmx,max( bot, top ) )

    endif
 enddo

 if (jased > 0 .and. zminrai == dmiss .and. .not.stm_included) then
    dlay = 0d0
    if (jaceneqtr == 1) then
       mx = ndxi
    else
       mx = size(grainlay,2)
    endif
    do k  = 1,mx
       if (jaceneqtr == 1) then
           xx = xz(k)
           yy = yz(k)
       else
           xx = xk(k)
           yy = yk(k)
       endif
       if ( inview(xx,yy) ) then
           dL = 0
           do j = 1,mxgr
              dL = dL + grainlay(j,k)
           enddo
           dlay = max(dlay, dL)
       endif
    enddo

    zmn = zmn - dlay

 endif


 if (zmn == zmx) then
    zmn = zmn -1d-3
    zmx = zmx + 1d-3
 else
    zmn = zmn - 1d-2*(zmx-zmn)
 endif

 if (xmn == xmx) then
    xmn = xmn -1d-3
    xmx = xmx + 1d-3
 endif

 if (nsiz == 1) then
    zmx = zmn + 1.2d0*(zmx-zmn)
 else
    zmx = zmn + 1.5d0*(zmx-zmn)
 endif


 if (zminrai .ne. -999) then
    zmn = zminrai
    zmx=max(zmn+1d-2, zmaxrai)
 endif

 if (md_ident == 'transport1d') then
    zmn = 0
    zmx = 30
 endif

 IF (YFAC > 0) THEN
    if (ymx .ne. ymn) then
       yfac = (zmx - zmn)/(ymx-ymn)
    else
       yfac = 0D-4
    endif
 ENDIF

 zmx2 = zmx + yfac*(ymx-ymn)
 if (zmx2 == zmn) zmx2 = zmn + 1


 if (nsiz == 1) then
    call setwor_rai(0.0,0.77,1.0,0.92, x1, zmn, x2, zmx2 )
 else  if (nsiz == 2) then
    call setwor_rai(0.0,0.56,1.0,0.92, x1, zmn, x2, zmx2 )
 else  if (nsiz == 3) then
    call setwor_rai(0.0,0.15,1.0,0.92, x1, zmn, x2, zmx2 )
 endif


 if (kmx > 0) then
    kplot = max(kplot,1)
    kplot = min(kplot,kmxn(nplot))
    kplotorg = kplot
    if (ndraw(28) > 3) then ! show node values


        if (ndraw(19) == 3) then

            if (.not. allocated(plotlin2) ) then
               allocate( plotlin2(lnkx), ip(lnkx), ip2(lnkx) )
            endif
            plotlin  = 0d0
            plotlin2 = 0d0
            ip       = 0
            ip2      = 0
            do LL = 1,lnx
               k1 = ln(1,LL)
               k2 = ln(2,LL)
               call getLbotLtop(LL,Lb,Lt)
               do L = Lb, Lt
                  kplot         = L - Lb + 1
                  plotlin (L)   = plotlin (L)   + znod(k1)
                  ip      (L)   = ip      (L)   + 1

                  kplot         = MAX( kplot - 1, 1)
                  plotlin (L-1) = plotlin (L-1) + znod(k1)
                  ip      (L-1) = ip      (L-1) + 1

                  plotlin2(L-1) = plotlin2(L-1) + znod(k2)
                  ip2     (L-1) = ip2     (L-1) + 1

                  kplot         = L - Lb + 1
                  plotlin2(L)   = plotlin2(L) + znod(k2)
                  ip2     (L)   = ip2     (L) + 1
               enddo
            enddo

            do LL = 1,lnx

               call getLbotLtop(LL,Lb,Lt)
               do L = Lb-1, Lt
                  if (ip(L) > 0) then
                      plotlin (L)  = plotlin (L) / ip(L)
                  endif
                  if (ip2(L) > 0) then
                      plotlin2(L)  = plotlin2(L) / ip2(L)
                  endif
               enddo
            enddo

            do LL = 1,lnx
               k1 = ln(1,LL)
               k2 = ln(2,LL)
               xp(1) = xz(k1)
               xp(2) = xp(1)
               xp(3) = xz(k2)
               xp(4) = xp(3)
               call getLbotLtop(LL,Lb,Lt)
               do L = Lb, Lt
                  k1 = ln(1,L)
                  k2 = ln(2,L)
                  yp(1) = zws(k1)
                  yp(2) = zws(k1-1)
                  yp(3) = zws(k2-1)
                  yp(4) = zws(k2)

                  zp(1) = plotlin(L)
                  zp(2) = plotlin(L-1)
                  zp(3) = plotlin2(L-1)
                  zp(4) = plotlin2(L)

                  call isofil(xp, yp, zp, 4, 0)
               enddo
            enddo

        else

           do n = 1,ndxi
              xxmn = minval( nd(n)%x )
              xxmx = maxval( nd(n)%x )
              xp(1) = xxmn
              xp(2) = xxmx
              xp(3) = xxmx
              xp(4) = xxmn
              kb = kbot(n)
              kt = ktop(n)
              do k = kb, kt

                 yp(1) = zws(k-1)
                 yp(2) = yp(1)
                 yp(3) = zws(k)
                 yp(4) = yp(3)

                 kplot = k - kb + 1
                 zn    = znod(n)
                 call isocol(zn, ncol)

                 if (ndraw(19) == 2) then
                    call dhtext( zn, xz(N), 0.5D0*(YP(1)+YP(3)) , 0.5D0*(YP(1)+YP(3)) )
                 else
                    call PFILLER(xp,yp,4,ncol, ncol )
                 endif

              enddo
           enddo

        endif
    endif

    if ( ndraw(29) > 1) then ! show link values
        do LL = 1,Lnx
           n1 = ln(1,LL)
           n2 = ln(2,LL)
           xp(1) = xz(n1)
           xp(4) = xp(1)
           xp(2) = xz(n2)
           xp(3) = xp(2)

           Lb = Lbot(LL)
           do L  = Lb , Ltop(LL)
              k1 = ln(1,L)
              k2 = ln(2,L)

              yp(1) = zws(k1-1)
              yp(2) = zws(k2-1)
              yp(3) = zws(k2)
              yp(4) = zws(k1)
              kplot = L - Lb + 1
              zn    = zlin(LL)

              call isocol2(zn, ncol)

              if (ndraw(11) == 2) then
                 xp1 = 0.25d0*(xp(1) + xp(2) + xp(3) + xp(4) )
                 yp1 = 0.25d0*(yp(1) + yp(2) + yp(3) + yp(4) )
                 call dhtext( zn, xp1, yp1, yp1 )
              else
                 call PFILLER(xp,yp,4,ncol, ncol )
              endif
           enddo
        enddo
    endif

    ncol = 221  ! markerpoint
    n = nplot
    k = kbot(n) + kplotorg - 1
      xxmn  = minval( nd(n)%x )
      xxmx  = maxval( nd(n)%x )
      xp(1) = 0.5d0*(xxmx + xxmn)
      yp(1) = 0.5d0*(zws(k) + zws(k-1))
      call cirr(xp(1), yp(1), ncol)
    ! xp(1) = xxmn ; xp(2) = xxmx ; xp(3) = xxmx ; xp(4) = xxmn
    ! yp(1) = zws(k-1)  ; yp(2) = yp(1)
    ! yp(3) = zws(k)    ; yp(4) = yp(3)
    ! call PFILLER(xp,yp,4,ncol, ncol )

    if ( NDRAW(2) > 0 ) then  ! draw layer interface lines in white

        do LL  = 1,lnxi
           n1  = ln(1,LL)
           n2  = ln(2,LL)
           xx  = xz(n1)
           xx2 = xz(n2)
           xxu = xu(LL)

           if (hu(LL) > 0) then
              Lb = Lbot(LL)
              Lt = Ltop(LL)
              do L = Lb, Lt
                 if (hu(L) > 0) then
                    k1 = ln(1,L)
                    k2 = ln(2,L)
                    call movabs(xx ,zws(k1))
                    if (ndraw(42) > 0) then 
                       zzu = min ( bob(1,LL), bob(2,LL) ) + hu(L)
                       call lnabs(xxu,zzu)
                    endif
                    call  lnabs(xx2,zws(k2))
                 endif
              enddo
           endif

        enddo

    endif

    if (jaanalytic == 0 ) then

       call setcol(ncolblack)
       CALL LINEWIDTH(2)
       do LL  = 1,lnxi
          if (hu(LL) > 0) then
             n1  = ln(1,LL)
             n2  = ln(2,LL)
             xx  = xz(n1)
             xx2 = xz(n2)
             call movabs(xx ,s1(n1))
             call  lnabs(xx2,s1(n2))
          endif
       enddo
    endif

    if ( NDRAW(2) .ge. 1) then
       call setcol(31)
       do LL  = 1, lnxi
          n1  = ln(1,LL)
          n2  = ln(2,LL)
          xx  = xz(n1)
          xx2 = xz(n2)
          call movabs(xx ,bl(n1))
          call  lnabs(xx2,bl(n2))
       enddo
    endif

    CALL LINEWIDTH(1)

    if ( NDRAW(13) .ge. 2) then
        call setcol(klvec)
        zfac = (zmx2-zmn)/(x2-x1)
        do n = 1,ndxi
           xp(1) = xz(n)
           do k = kbot(n),ktop(n)

              uu    = ucx(k)
              ww    = 0.5d0*( ww1(k) + ww1(k-1))
              yp(1) = 0.5d0*(zws(k)+zws(k-1))
              call arrowsxyzfac( xp(1), yp(1), uu, ww, VFAC, 0, zfac)

           enddo

        enddo

    endif

    kplot = kplotorg
 endif

 if ( NDRAW(2) .ge. 1) then
    call tekrailinesbathy(31,0,1) ! bl
 endif

 if (jased > 0 .and. jased < 4) then
    do j = 1,mxgr
       call tekrailinesbathy(15,0,1+j) ! grainlay 1,2 etc
    enddo
 endif

 if (jagrw >= 1) then
    call tekrailines(ncolln,1,4) ! pgrw
    call tekrailines(ncolln,1,5) ! pgrw
 endif

 if (md_ident == 'transport1d' .or. jasal == 1 .and. ( md_ident == 'wetbed' .or. md_ident == 'wetbed' ) ) then
    call tekrailines(221,1,3) ! sa1
 endif

 if (kmx == 0) then
    call tekrailines(221,1,1) ! s1
    if (nonlin >= 2) then
       call tekrailines(ncolana,1,6) ! s1m
       ! call tekrailines(2,0,2)       ! bob
       ! call tekrailines(2,0,7)       ! bbb
    endif
 endif

 if (ndraw(30) == 5) then
    call setcol(2)
    do LL = 1,lnx
       n1  = ln(1,LL)  ; n2  = ln(2,LL)
       xz1 = xz(n1)    ; xz2 = xz(n2)
       do L = Lbot(LL) , Ltop(LL)
          k1 = ln(1,L) ; k2 = ln(2,L)
          zz1 = 0.5d0*( zws(k1) + zws(k1-1) )
          zz2 = 0.5d0*( zws(k2) + zws(k2-1) )
          call movabs(xz1, zz1 )
          call  lnabs(xz2, zz2 )
       enddo
    enddo
 endif


 call setcol(ncolblack) ! NCOLANA)
 ! call LINEWIDTH(2)

 if (md_IDENT == 'transport1d') then
     call tektransport1D(time1-tstart_user)
     call setcol(3)
     call movabs(xmn, 0d0)
     call lnabs( xmx, 0d0)
     !call htext( 1d0, xmx, 1d0)
 else if (md_IDENT == 'carrier') then
     call carrier(ndx,time1-tstart_user)
 else if (md_IDENT(1:6) == 'drybed') then
     call drybed(time1-tstart_user)
 else if (md_IDENT(1:6) == 'wetbed') then
     call wetbed(time1-tstart_user)
 else if (md_IDENT(1:12) == 'coriolistilt') then
     call coriolistilt(time1-tstart_user)
 else if (md_IDENT(1:14) == 'corioliskelvin') then
     call corioliskelvin(time1-tstart_user)
 else if (index(md_ident,'thacker1d') > 0) then
    call thacker1d(0,xz,yz,s1,bl,ndx,time1-tstart_user)
 else if (md_IDENT == 'equator1d') then
     call equatorial(time1-tstart_user)
 else if (md_IDENT(1:8) == 'belanger') then
     call belanger()
 endif
 ! call LINEWIDTH(1)

 do ng = 1,ngatesg  ! loop over gate signals, tekrai
    zgaten = zgate(ng)
    do n  = L1gatesg(ng), L2gatesg(ng)
       L  = kgate(3,n) ; k1 = ln(1,L) ; k2 = ln(2,L)
       bup = min( bob(1,L), bob(2,L) )
       call fbox(xz(k1),zgaten,xz(k2),zgaten+20d0)
       ! call fbox(xz(k1),bup   ,xz(k2),bup-10d0)
    enddo
 enddo

 do ng = 1,ncgensg  ! loop over gate signals, tekrai
    zgaten = zcgen(3*ng-1)
    do n   = L1cgensg(ng), L2cgensg(ng)
       k1       = kcgen(1,n)
       k2       = kcgen(2,n)
       L        = kcgen(3,n)

       bup = min( bob(1,L), bob(2,L) )
       doorh = 10d0
       if (generalstruc(ng)%gatedoorheight < 1d10 .and. generalstruc(ng)%gatedoorheight < 1d10) then
         if (generalstruc(ng)%gatedoorheight > 0d0) doorh = generalstruc(ng)%gatedoorheight
         call fbox(xz(k1),zgaten,xz(k2),zgaten+doorh)
       end if

       call fbox(xz(k1),bup   ,xz(k2),zmn) ! bup-10d0
    enddo
 enddo

 do ng = 1,ncdamsg  ! loop over gate signals, tekrai
    do n  = L1cdamsg(ng), L2cdamsg(ng)
       L  = kcdam(3,n) ; k1 = ln(1,L) ; k2 = ln(2,L)
       bup = bob(2,L)  ! min( bob(1,L), bob(2,L) )
       call fbox(xz(k1),bup   ,xz(k2),bup-10d0)
    enddo
 enddo

 call setcol(121)
 if (kmx > 0) then
   do n = 1,numsrc                            ! teksorsin rai
     qsrck  = qsrc(n)
     kk     = ksrc(1,n)                      ! 2D pressure cell nr from
     if (kk .ne. 0 .and.  ksrc(2,n) > 0 ) then
        xp(1) = xz(kk)
        bup   = 0.1d0*sqrt(ba(kk))
        do k = ksrc(2,n), ksrc(3,n)
           yp(1) = 0.5d0*( zws(k) + zws(k-1) )
           ! CALL KCIR(XP(1),YP(1),qsrck)
           call fbox(xz(kk)-bup, zws(k-1), xz(kk)+bup , zws(k) )
        enddo
     endif

     kk     = ksrc(4,n)                      ! 2D pressure cell nr to
     if (kk .ne. 0 .and.  ksrc(5,n) > 0) then
        xp(1) = xz(kk)
        bup   = 0.1d0*sqrt(ba(kk))
        do k = ksrc(5,n), ksrc(6,n)
           yp(1) = 0.5d0*( zws(k) + zws(k-1) )
           ! CALL KCIR(XP(1),YP(1),qsrck)
           call fbox(xz(kk)-bup, zws(k-1), xz(kk)+bup , zws(k) )
        enddo
     endif
   enddo
 endif

 if (javeg > 0 .and. kmx > 0) then
    call setcol(221)
    do k = 1,ndxi
       if (stemheight(k) > 0d0) then
          call movabs( xz(k), zws(kbot(k)-1) )
          xx = stemheight(k) * sin(phiv(k))
          yy = stemheight(k) * cos(phiv(k))
          call  lnabs( xz(k)+xx, zws(kbot(k)-1)+yy )
       endif
    enddo
 endif


 call viewport(0.0,0.0,1.0,1.0)

 if (nsiz > 1 .and. jtextflow > 0) then
    ! assen in 'gewone' aspect=1 wereld coordinaten, anders wordt de text plat afgedrukt in interacter
 !   CALL IGrUnits (0.0,0.0,1.0,1.0)
    CALL setwor(0d0,0d0,1d0,1d0)

    call setcol(3) ! zwart
    zz = 0.05*(zmx-zmn)/nsiz
    call htext_rai( zmn         , x1+12d0*rcir, zmn-2d0*zz,rcir,zz,1)
    call htext_rai( zmx         , x1+12d0*rcir, zmx       ,rcir,zz,1)
    call htext_rai( x1+10d0*rcir, x1+12d0*rcir, zmn-2d0*zz,rcir,zz,2)
    call htext_rai( x2-10d0*rcir, x2-12d0*rcir, zmn-2d0*zz,rcir,zz,2)
 endif

kplotfrombedorsurface = kplotfrombedorsurfacesav
 call setwor(x1,y1,x2,y2)

 return
 end subroutine tekrai
