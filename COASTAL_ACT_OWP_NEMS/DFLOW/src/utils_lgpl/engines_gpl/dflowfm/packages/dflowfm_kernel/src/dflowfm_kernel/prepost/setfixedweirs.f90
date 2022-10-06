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

! $Id: setfixedweirs.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/setfixedweirs.f90 $

subroutine setfixedweirs()      ! override bobs along pliz's, jadykes == 0: only heights, 1 = also dyke attributes
 use m_netw
 use m_flowgeom
 use m_flow
 use m_missing
 use m_alloc
 use unstruc_model
 use timespace
 use unstruc_messages
 use m_fixedweirs
 use kdtree2Factory
 use m_sferic
 use m_polygon
 use m_partitioninfo
 use string_module, only: strsplit, get_dirsep
 use geometry_module, only: dbdistance, CROSSinbox, dcosphi, duitpl, normalout
 use unstruc_caching

 implicit none

 integer                       :: np, k, kk, n1, n2, n12, n, nn, L, LL, ja,  jacros, minp, kint, ierr, nt, nh, nhh, i, Lf
 integer                       :: jaweir, Lastfoundk, kf, kL, jarestorepol, Lnt, k1, nna, nnb, nl1, nl2, k3, k4
 integer         , allocatable :: iwu(:), ihu(:)
 double precision              :: SL, SM, XCR, YCR, CRP, Xa, Ya, Xb, Yb, zc, zh, zhu, zhd, af, dz1, dz2, xn, yn, adjacentbob, cosphi, sig, bobL
 double precision, allocatable :: csh(:), snh(:), zcrest(:), dzsillu(:), dzsilld(:), crestlen(:), taludu(:), taludd(:), vegetat(:),ztoeu(:),ztoed(:)
 integer         , allocatable :: iweirtyp(:)
 integer         , allocatable :: ifirstweir(:)

 double precision, dimension(:), allocatable :: dSL
 integer,          dimension(:), allocatable :: iLink
 integer,          dimension(:), allocatable :: iLcr ! link crossed yes no
 integer,          dimension(:), allocatable :: iPol

 integer                                     :: iL, numLL, numcrossedLinks, ii, LLL, LLLa, nx
 integer                                     :: mout, jatabellenboekorvillemonte
 integer                                     :: ierror

 integer                                     :: jakdtree=1
 character(len=5)                            :: sd
 character(len=200), dimension(:), allocatable       :: fnames
 integer                                     :: jadoorladen, ifil
 double precision                            :: t0, t1, t_extra(2,10), BLmn
 character(len=128)                          :: mesg


 if ( len_trim(md_fixedweirfile) == 0 ) then
    ifixedweirscheme = 0
    return
 endif

 jatabellenboekorvillemonte = 0
 if (ifixedweirscheme == 8) jatabellenboekorvillemonte = 1
 if (ifixedweirscheme == 9) jatabellenboekorvillemonte = 2


 call readyy('Setfixedweirs', 0d0)

 !if (allocated(csh) ) then
 !   deallocate(ihu, csh, snh, zcrest, dzsillu, dzsilld, crestlen, taludu, taludd, vegetat) ! (10 arrays)
 !endif

 allocate (ihu(lnx))      ; ihu = 0
 allocate (csh(lnx))      ; csh = 0d0
 allocate (snh(lnx))      ; snh = 0d0
 allocate (zcrest(lnx))   ; zcrest = -1000d0   ! starting from a low value
 allocate (dzsillu(lnx))  ; dzsillu = 0d0
 allocate (dzsilld(lnx))  ; dzsilld = 0d0
 allocate (ztoeu(lnx))    ; ztoeu = 1000d0  ! starting from a high value
 allocate (ztoed(lnx))    ; ztoed = 1000d0  ! starting from a high value
 allocate (crestlen(lnx)) ; crestlen = 3d0
 allocate (taludu(lnx))   ; taludu = 4d0
 allocate (taludd(lnx))   ; taludd = 4d0
 allocate (vegetat(lnx))  ; vegetat = 0d0
 allocate (iweirtyp(lnx)) ; iweirtyp = 0
 allocate (ifirstweir(lnx)) ; ifirstweir = 1                       ! added to check whether fixed weir data is set for the first time at a net link (1=true, 0=false)

 call klok(t0)
 t_extra(1,1) = t0

 ! Load fixed weirs polygons from file.
 ! --------------------------------------------------------------------
 if (len_trim(md_fixedweirfile) > 0) then
    call strsplit(md_fixedweirfile,1,fnames,1)
    jadoorladen = 0
    do ifil=1,size(fnames)
       call oldfil(minp, fnames(ifil))
       N1  = index (fnames(ifil), get_dirsep(), .true.)
       !  fix for Linux-prepared input on Windows
       if ( N1.eq.0 ) then
            N1 = index(fnames(ifil), char(47), .true.)
       end if

       sd  = ''
       if (jampi == 1) then
           sd = '_'//trim(sdmn)
       end if

       N2  = INDEX (fnames(ifil), '.' , .true.)
       if (N2 == 0) then
           N2 = len_trim(fnames(ifil))
       else
           N2 = N2 - 1
       end if
       if (jawriteDFMinterpretedvalues > 0) then
          call newfil(mout, trim(getoutputdir())//'DFM_interpreted_fxwvalues_'//fnames(ifil)(n1+1:n2)//trim(sd)//'.xyz')
          write (mout, '(a)') '* xu yu crest(bob) width(wu) xk3 yk3 xk4 yk4'
       end if
       call reapol(minp, jadoorladen)
       jadoorladen = 1
    enddo
    call klok(t_extra(2,1))
    call klok(t_extra(1,2))
    call pol_to_flowlinks(xpl, ypl, zpl, npl, nfxw, fxw)
    call klok(t_extra(2,2))
    deallocate(fnames)
 end if

 kint = max(lnxi/1000,1)

 call klok(t_extra(1,3))
 allocate (iLink(Lnx))
 allocate (iLcr(Lnx))     ; Ilcr = 0
 allocate (ipol(Lnx))
 allocate (dSL(Lnx))
 if ( cacheRetrieved() ) then
    ierror = 0
    call copyCachedFixedWeirs( npl, xpl, ypl, numcrossedLinks, iLink, iPol, dSL, success )
 else
    success = .false.
 endif
 if ( .not. success ) then
    call find_crossed_links_kdtree2(treeglob,NPL,XPL,YPL,2,Lnx,1,numcrossedLinks, iLink, iPol, dSL, ierror)
    call cacheFixedWeirs( npl, xpl, ypl, numcrossedLinks, iLink, iPol, dSL )
 endif
 call klok(t_extra(2,3))

 call klok(t_extra(1,4))
 if ( ierror == 0) then
    do iL = 1,numcrossedlinks
       L  = iLink(il)
       iLcr(L) = 1
    enddo
 else
    n = 0 ; Lastfoundk = 0
    do L = 1,lnxi

       if (mod(L,kint) == 0) then
           AF = dble(L)/dble(lnxi)
           call readyy('Setfixedweirs', af )
       endif

       n1 = ln(1,L) ; n2 = ln(2,L)
       xa = xz(n1)  ; ya = yz(n1)
       xb = xz(n2)  ; yb = yz(n2)

       iloop:do i = 1,2

           if (i == 1) then
              if (Lastfoundk == 0) cycle
              kf = max(1,     Lastfoundk - 100)
              kL = min(npl-1, Lastfoundk + 100)
           else
              kf = 1
              kL = npl-1
           endif

           Lastfoundk = 0
           do k = kf,kL

               if (xpl(k) .ne. dmiss .and. xpl(k+1) .ne. dmiss) then
                   CALL CROSSinbox (XPL(k), YPL(k), XPL(k+1), YPL(k+1), Xa, Ya, Xb, Yb, jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)

                   if ( jacros.eq.1 ) then
                      Lastfoundk = k
                      n = n + 1
                      ilink(n) = L
                      ipol(n)  = k
                      dsl(n)   = SL
                      iLcr(L)  = 1
                      exit iloop
                   end if
               endif
           enddo

       enddo iloop

    enddo
    numcrossedlinks = n
 endif
 call klok(t_extra(2,4))

 call klok(t1)
 write(mesg,"('fixed weirs with kdtree2, elapsed time: ', G15.5, 's.')") t1-t0
 call mess(LEVEL_INFO, trim(mesg))
 write(mesg,"('fixed weirs: read files,  elapsed time: ', G15.5, 's.')") t_extra(2,1)-t_extra(1,1)
 call mess(LEVEL_INFO, trim(mesg))
 write(mesg,"('fixed weirs: pol_to_flowlinks, elapsed time: ', G15.5, 's.')") t_extra(2,2)-t_extra(1,2)
 call mess(LEVEL_INFO, trim(mesg))
 write(mesg,"('fixed weirs: find_crossed_links, elapsed time: ', G15.5, 's.')") t_extra(2,3)-t_extra(1,3)
 call mess(LEVEL_INFO, trim(mesg))
 write(mesg,"('fixed weirs: attributes,  elapsed time: ', G15.5, 's.')") t_extra(2,4)-t_extra(1,4)
 call mess(LEVEL_INFO, trim(mesg))

 nh = 0
 do iL = 1,numcrossedlinks

    L  = ilink(iL)
    k  = ipol(iL)
    SL = dsl(iL)
    n1 = ln(1,L) ; n2 = ln(2,L)

    if (kcu(L) .eq. 1 .or. kcu(L) == 5) then
       cycle ! UNST-2226: test code for forbidding fixed weirs on 1D
    end if

    zc = sl*zpL(k+1) + (1d0-sl)*zpL(k)

    if (abs(kcu(L)) == 2) then
       bobL = min(bob(1,L), bob(2,L))
    else
       bobL = max(bob(1,L), bob(2,L))
    endif

    ! if ( (zc > bobL .and. zc > zcrest(L)) .or. ( (ifixedweirscheme == 8 .or. ifixedweirscheme == 9) .and. ifirstweir(L) == 1) ) then   ! For Villemonte and Tabellenboek fixed weirs under bed level are also possible

    if ( (zc > bobL .and. zc > zcrest(L)) .or. ifirstweir(L) == 1 ) then   ! For Villemonte and Tabellenboek fixed weirs under bed level are also possible


       ! Set whether this is the first time that for this link weir values are set:
       ! As a result, only the first fixed weir under the bed level is used
       ifirstweir(L) = 0

       bob(1,L) = max(bob(1,L), zc) ; bob(2,L) = max(bob(2,L), zc)


       if (kcu(L) .ne. 2 .and. kcu(L) .ne. 1) then
          cycle  ! weirs only on regular links
       endif

       jaweir   = 1 ! 0
       if (ifixedweirscheme > 0) then
          if (jakol45 == 0) then ! no dzl or dzr specified
             ! jaweir = 1
          else
             dz1   = sl*dzL(k+1) + (1d0-sl)*dzL(k)
             dz2   = sl*dzR(k+1) + (1d0-sl)*dzR(k)

             ! if (min (dz1,dz2) >= sillheightmin) then  ! weir if sufficiently high and regular link
             !   jaweir = 1
             !elseif (ifixedweirscheme == 8 .or. ifixedweirscheme == 9) then
                ! For Villemonte and Tabellenboek weirs with low sills are also applied, in order to be consistent with Simona
             !   jaweir = 1
             !endif

             if (jaconveyance2D >=1) then   ! now set adjacent bobs of netlinks | sufficiently perpendicular to fixedweir to local ground level
                do i = 1,2
                   n1 = lncn(i,L)
                   do kk  = 1, nmk(n1)                                  !          |         |
                      Lnt = nod(n1)%lin(kk)                             ! ---------o---------o-------fixedweir
                      Lf = lne2ln(Lnt)                                  !          |         |
                      if (Lf == 0) cycle
                      if (iLcr(abs(Lf)) == 1) cycle
                      nna = kn(1,Lnt)
                      nnb = kn(2,Lnt)
                      xa  = xk(nna) ; ya = yk(nna)
                      xb  = xk(nnb) ; yb = yk(nnb)

                      COSPHI = DCOSPHI(Xpl(k), Ypl(k), xpl(k+1), ypl(k+1), xa, ya, xb, yb, jsferic, jasfer3D, dxymis)
                      if (abs(cosphi) < 0.5d0) then
                          if (nna .ne. n1) then
                              nhh = nna
                              nna = nnb
                              nnb = nhh
                          endif  ! na is now basepoint
                          xa = xk(nna) ; ya = yk(nna)
                          xb = xk(nnb) ; yb = yk(nnb)
                          call duitpl(Xpl(k), Ypl(k), xpl(k+1), ypl(k+1), xa, ya, xb, yb, sig, jsferic)
                          adjacentbob = dmiss
                          if (sig > 0 ) then
                              if (dz2 > 3d0 .and. dz1 < 3d0) then  ! kade at other side deeper than 3 m
                                 adjacentbob = zc - dz1            ! then set kade ground level
                              endif
                          else
                              if (dz1 > 3d0 .and. dz2 < 3d0) then
                                 adjacentbob = zc - dz2
                              endif
                          endif

                          if (Lf > 0 .and. adjacentbob .ne. dmiss) then
                             if (jaconveyance2D >= 1) then 
                                if (lncn(1,Lf) == n1) then
                                    bob(1,Lf) = adjacentbob
                                else
                                    bob(2,Lf) = adjacentbob
                                endif
                             else 
                                 bob(1,Lf) = adjacentbob
                                 bob(2,Lf) = adjacentbob
                             endif
                             !nl1 = ln(1,Lf) ; nl2 = ln(2,Lf)
                             !bl(nl1) = min(bl(nl1), adjacentbob )
                             !bl(nl2) = min(bl(nl2), adjacentbob ) ! still needs to be done 
                          endif

                      endif

                   enddo

                enddo !1,2
             endif
          endif
       endif

       if (jaweir > 0) then                                      ! set weir treatment
          ihu(L) = k
          call normalout( XPL(k), YPL(k), XPL(k+1), YPL(k+1) , xn, yn, jsferic, jasfer3D, dmiss, dxymis)
          
          k3 = lncn(1,L) ; k4 = lncn(2,L)
          wu(L) = dbdistance ( xk(k3), yk(k3), xk(k4), yk(k4), jsferic, jasfer3D, dmiss)  ! set 2D link width
          
          wu(L) = wu(L) * abs( xn*csu(L) + yn*snu(L) )           ! projected length of fixed weir


          if (jakol45 == 2) then                                 ! use local type definition
             !
             ! recompute ground heights if zc is larger than previous crest levels
             !
             if (zc .gt. zcrest(L)) then
                dzsillu(L)  = zc - ztoeu(L)
                dzsilld(L)  = zc - ztoed(L)
             endif
             !
             zcrest(L)  = zc
             zhu = (1d0-sl)*dzl(k) + sl*dzl(k+1)                 ! ground height left
             zhd = (1d0-sl)*dzr(k) + sl*dzr(k+1)                 ! ground height right
             crestlen(L) = (1d0-sl)*dcrest(k) + sl*dcrest(k+1)   ! crest length
             taludu(L)   = (1d0-sl)*dtl(k)    + sl*dtl(k+1)      ! talud at ln(1,L)
             taludd(L)   = (1d0-sl)*dtr(k)    + sl*dtr(k+1)      ! talud at ln(2,L)
             vegetat(L)  = (1d0-sl)*dveg(k)   + sl*dveg(k+1)     ! vegetation on fixed weir
             iweirtyp(L) = iweirt(k)                             ! type of weir
             if (iweirt(k) .eq. 1 ) then
                 iadv(L) = 24 ; jatabellenboekorvillemonte = 1   !  Tabellenboek
             else if (iweirt(k) .eq. 2 ) then
                 iadv(L) = 25 ; jatabellenboekorvillemonte = 1   !  Villemonte
             else
                 iadv(L) = 21                                    !  Subgrid, ifixedweirscheme = 6 or 7
             endif
             !
             ! If link is reversed, exchange ground height levels and taluds
             !
             if (xn*csu(L) + yn*snu(L) < 0d0) then  ! check left/right
                 zh = taludd(L)  ; taludd(L)  = taludu(L)  ; taludu(L)  = zh
                 zh = zhd; zhd = zhu;  zhu = zh
             endif
             !
             ! lowest toe is applied
             !
             if (zc-zhu .lt. ztoeu(L)) then
                ztoeu(L)   = zc - zhu
                dzsillu(L)  = zcrest(L) - ztoeu(L)
             endif
             if (zc-zhd .lt. ztoed(L)) then
                ztoed(L)   = zc - zhd
                dzsilld(L)  = zcrest(L) - ztoed(L)
             endif
             !! write (msgbuf,'(a,2i5,7f10.3)') 'Projected fixed weir', L, iweirtyp(L), zc, bobL, dzsillu(L), dzsilld(L),crestlen(L),taludu(L),taludd(L); call msg_flush()
          else                                                       ! use global type definition
             if (ifixedweirscheme == 7) then
                iadv(L)    = 23    !  Rajaratnam
             else if (ifixedweirscheme == 8) then
                iadv(L)    = 24    !  Tabellenboek
                dzsillu(L) = zc - bl(n1) ; dzsilld(L) = zc - bl(n2)  ! if not specified then estimate
             else if (ifixedweirscheme == 9) then
                iadv(L)    = 25    !  Villemonte
                dzsillu(L) = max(0d0, zc - bl(n1) ) ; dzsilld(L) = max(0d0, zc - bl(n2) ) ! if not specified then estimate
             else
                iadv(L)    = 21    !  Ifixedweirscheme 6
             endif
          endif

 
          ! 21 = Ifixedweirscheme 6
          ! 22 = General structure
          ! 23 = Rajaratnam
          ! 24 = Tabellenboek
          ! 25 = Villemonte

          if (jawriteDFMinterpretedvalues > 0) then
             write (mout, '(8(f24.4))') xu(L), yu(L), bob(1,L), fixedweircontraction*wu(L), xk(k3), yk(k3), xk(k4), yk(k4)
          end if
       else
          nh = nh + 1                                            ! just raised bobs
       endif
    else
       if ( ifirstweir(L) == 0 .and. jakol45 == 2) then !  .and. (ifixedweirscheme == 8 .or. ifixedweirscheme == 9) ) then   !  only for fixed weirs under the bed level for Tabellenboek or Villemonte and not for the first time that a fixed weir is set on this link
          ! check for larger ground height height values if at this link already a fixed weir exist

          !
          ! check whether crestlevel is higher
          !
          if (zc > zcrest(L)) then
             zcrest(L) = zc
             !! write (msgbuf,'(a,i5,f10.3)') 'Higher crest level: ', L,  zcrest(L); call msg_flush()
          endif
          if (jakol45/=0) then
             call normalout( XPL(k), YPL(k), XPL(k+1), YPL(k+1) , xn, yn, jsferic, jasfer3D, dmiss, dxymis)  ! test EdG
             zhu =  (1d0-sl)*dzl(k) + sl*dzl(k+1)
             zhd =  (1d0-sl)*dzR(k) + sl*dzR(k+1)
             if (xn*csu(L) + yn*snu(L) < 0d0) then  ! check left/right
                zh = zhd; zhd = zhu;  zhu = zh
             endif
            !
            ! Check whether toe is lower. If so, also adjust toe level and the ground height
            ! If ground height is smaller than 1 cm, then this neglected
            !
            if (zc-zhu .lt. ztoeu(L) .and. zhu .gt. 0.01) then 
               ztoeu(L)   = zc - zhu                            
               dzsillu(L) = zcrest(L) - ztoeu(L)              
               !! write (msgbuf,'(a,i5,f10.3)') 'Larger sill up:     ', L,  dzsillu(L); call msg_flush()
            endif
            if (zc-zhd .lt. ztoed(L) .and. zhd .gt. 0.01) then
               ztoed(L)   = zc - zhd
               dzsilld(L) = zcrest(L) - ztoed(L)
               !! write (msgbuf,'(a,i5,f10.3)') 'Larger sill down:   ', L, dzsilld(L); call msg_flush()
            endif
          endif
       endif
    endif
    !! write (msgbuf,'(a,2i5,7f10.3)') 'Projected fixed weir', L, iweirtyp(L), zcrest(L), ztoeu(L), dzsillu(L),ztoed(L),dzsilld(L),taludu(L),taludd(L); call msg_flush()

 enddo
 if (jawriteDFMinterpretedvalues > 0) then
    call doclose(mout)
 end if

 if (jakol45 == 2 .and. sillheightmin > 0d0) then  ! when a minimum threshold is specified    
                                                   ! and toe heights are known, and agreed upon 
    do L = 1,lnxi
       if (ihu(L) > 0) then                        ! when flagged as weir
 
           do ii = 1,2                             ! loop over adjacent cells
              k = 0
              if (ii == 1 .and. dzsillu(L) < sillheightmin .and. dzsilld(L) > sillheightmin .or. & 
                  ii == 2 .and. dzsilld(L) < sillheightmin .and. dzsillu(L) > sillheightmin ) then
                  k = ln(ii,L) 
              endif
              if (k > 0) then                                          ! flatland on node k 
                  nx      = nd(k)%lnx 
                  do LL   = 1, nx                                      ! loop over all attached links
                     LLL  = nd(k)%ln(LL) ; LLLa = iabs(LLL)
                     if (LLLa == L) then                                
                         LLLa = LL-1 ; if (LLLa == 0) LLLa = nx        ! left of weir link
                         LLLa = iabs(nd(k)%ln(LLLa))
                         if (ihu(LLLa) == 0) then                      ! if not already marked as weir 
                            bob(1,LLLa) = max( zcrest(L),bob(1,LLLa) ) ! raise both bobs 
                            bob(2,LLLa) = max( zcrest(L),bob(2,LLLa) ) ! raise both bobs
                         endif
                         LLLa = LL+1 ; if (LLLa > nx) LLLa = 1         ! right of weir link
                         LLLa = iabs(nd(k)%ln(LLLa))
                         if (ihu(LLLa) == 0) then                      ! if not already marked as weir 
                            bob(1,LLLa) = max( zcrest(L),bob(1,LLLa) ) ! raise both bobs 
                            bob(2,LLLa) = max( zcrest(L),bob(2,LLLa) ) ! raise both bobs o
                         endif
                     endif
                  enddo
               endif
          enddo 
       endif
    enddo

    BL = 1d9 
    do L = 1,lnx                         ! switch off weirs that do not need weir treatment
       if ( ihu(L) > 0 .and. (dzsillu(L) < sillheightmin .or. dzsilld(L) < sillheightmin) ) then
          ihu(L) = 0 ; iadv(L) = iadvec
          if (slopedrop2D > 0d0) then 
             iadv(L) = 8
          endif
       endif
       BLmn   = min( bob(1,L),bob(2,L) ) ! and reset BL to lowest attached link
       n1     = ln(1,L) ; n2 = ln(2,L)
       BL(n1) = min(BL(n1),BLmn) 
       BL(n2) = min(BL(n2),BLmn) 
    enddo
 
 endif

 nfxw = 0
 do L = 1,lnxi
    if ( ihu(L) > 0) then
       nfxw = nfxw + 1 ! TODO: HK: incorrect/inconsistent use of nfxw: upon reading the pliz file it is nr of polylines, now it becomes the total number of flow links crossed by a fixed weir.

       if ( iadv(L) == 21) then
           call setfixedweirscheme3onlink(L)
           if (ifixedweirscheme == 7) then
               iadv(L) = 23
           endif
       endif

    endif
 enddo

 if (nfxw > 0) then
    if (allocated (lnfxw) )   deallocate(nfxwL,lnfxw)
    if (allocated (weirdte) ) deallocate(weirdte)
    if (allocated (shlxw) )   deallocate(shlxw,shrxw,crestlevxw,crestlxw,taludlxw,taludrxw,vegxw,iweirtxw)
    allocate ( nfxwL(Lnx) ,stat=ierr)
    call aerr('nfxwL(Lnx)',ierr,lnx)

    call realloc(weirdte, nfxw, keepExisting=.false., fill=0d0, stat=ierr)
    call aerr  ('weirdte', ierr, nfxw)
    allocate ( lnfxw(nfxw) ,stat=ierr)
    call aerr('lnfxw(nfxw)',ierr,nfxw)
    allocate ( shlxw(nfxw) ,stat=ierr)   ! Tabellenboek / Villemonte parameters)
    call aerr('shlxw(nfxw)',ierr,nfxw)
    allocate ( shrxw(nfxw) ,stat=ierr)
    call aerr('shrxw(nfxw)',ierr,nfxw)
    allocate ( crestlevxw(nfxw) ,stat=ierr)
    call aerr('crestlevxw(nfxw)',ierr,nfxw)
    allocate ( crestlxw(nfxw) ,stat=ierr)
    call aerr('crestlxw(nfxw)',ierr,nfxw)
    allocate ( taludlxw(nfxw) ,stat=ierr)
    call aerr('taludlxw(nfxw)',ierr,nfxw)
    allocate ( taludrxw(nfxw) ,stat=ierr)
    call aerr('taludrxw(nfxw)',ierr,nfxw)
    allocate ( vegxw(nfxw) ,stat=ierr)
    call aerr('vegxw(nfxw)',ierr,nfxw)
    allocate ( iweirtxw(nfxw) ,stat=ierr)
    call aerr('iweirtxw(nfxw)',ierr,nfxw)
 endif

 nfxw = 0
 do L = 1,lnxi
    if ( ihu(L) > 0) then
       nfxw = nfxw + 1
       lnfxw(nfxw)    = L
       nfxwL(L)       = nfxw
       crestlevxw(nfxw) = zcrest(L)
       shlxw(nfxw)    = dzsillu(L)
       if (ifixedweirscheme == 8 .or. ifixedweirscheme == 9) then
          shlxw(nfxw) = max (0.1d0, shlxw(nfxw) )  !  in case of the Tabellenboek and Villemonte the ground height left should be at least 0.1 m, as in Simona and Delft3D-FLOW
       endif
       shrxw(nfxw)    = dzsilld(L)
       if (ifixedweirscheme == 8 .or. ifixedweirscheme == 9) then
          shrxw(nfxw) = max (0.1d0, shrxw(nfxw) )  !  in case of the Tabellenboek and Villemonte the ground height right should be at least 0.1 m, as in Simona and Delft3D-FLOW
       endif
       crestlxw(nfxw) = crestlen(L)
       taludlxw(nfxw) = taludu(L)
       taludrxw(nfxw) = taludd(L)
       vegxw(nfxw)    = vegetat(L)
       iweirtxw(nfxw) = iweirtyp(L)
    endif
 enddo

 deallocate(ihu, csh, snh, zcrest, dzsillu, dzsilld, crestlen, taludu, taludd, vegetat, iweirtyp, ztoeu, ztoed)
 if (jatabellenboekorvillemonte == 0 .and. jashp_fxw == 0 .and. allocated(shlxw) ) then
    deallocate(shlxw, shrxw, crestlevxw, crestlxw, taludlxw, taludrxw, vegxw, iweirtxw)
 endif

 do i = 1, nfxw
    L = lnfxw(i)
    if (L > 0) then
       wu(L)    = wu(L) * fixedweircontraction ! TODO: EdG/HK: this will be wrong if MULTIPLE fixed weirs are snapped onto the same flow link (repeated multiplication by fixedweircontraction)
    endif
 enddo

 call doclose(minp)

 if (nfxw > 0) then
    call mess(LEVEL_INFO,'Number of flow Links with fixed weirs :: ', nfxw)
 endif
 if (nh > 0) then
    call mess(LEVEL_INFO,'Number of flow Links with highlines :: ', nh)
 endif

 call readyy(' ', -1d0 )


1234 continue

! deallocate
 if ( jakdtree.eq.1 ) then
    if ( allocated(iLink) ) deallocate(iLink)
    if ( allocated(iPol)  ) deallocate(iPol)
    if ( allocated(dSL)   ) deallocate(dSL)
 end if

end subroutine setfixedweirs