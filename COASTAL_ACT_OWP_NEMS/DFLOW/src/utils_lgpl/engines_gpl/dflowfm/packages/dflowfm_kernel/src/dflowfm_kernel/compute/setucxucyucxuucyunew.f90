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

! $Id: setucxucyucxuucyunew.f90 140640 2022-01-18 11:11:27Z jagers $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/setucxucyucxuucyunew.f90 $

 subroutine setucxucyucxuucyunew()   ! and reclaim ucxq, ucyq for Coriolis without interfering with Morfology
 use m_flowgeom
 use m_flow
 use m_sobekdfm
 use m_sediment, only: jased, stm_included
 use m_missing
 use m_flowparameters, only: jabarrieradvection
 use m_sferic
 implicit none

 integer          :: L, KK, k1, k2, k, nw, Lb, Lt, LL, nn, n, kt,kb, kbk, k2k, n1, n2, ip, i12, La, nx
 integer          :: itpbn, newucxq=0
 double precision :: uu, vv, uucx, uucy, wcxu, wcyu, cs, sn, adx, ac1, ac2, wuw, hdx, hul, hsk, uin, duxdn, duydn, uhu, htrs
 double precision :: dischcorrection
 double precision :: uinx, uiny, ahu, uxy

 double precision,  allocatable :: husx, husy

 double precision, external :: nod2linx, nod2liny
 double precision, external :: lin2nodx, lin2nody

 ucxq = 0d0 ; ucyq = 0d0           ! zero arrays
 
 if (iperot /= -1) then
 ucx  = 0d0 ; ucy  = 0d0

 if (kmx < 1) then                                   ! original 2D coding

    do L = 1,lnx1D
       if (u1(L) .ne. 0d0 .and. kcu(L) .ne. 3) then  ! link flows ; in 2D, the loop is split to save kcu check in 2D
          k1 = ln(1,L) ; k2 = ln(2,L)
          ucx(k1) = ucx(k1) + wcx1(L)*u1(L)
          ucy(k1) = ucy(k1) + wcy1(L)*u1(L)
          ucx(k2) = ucx(k2) + wcx2(L)*u1(L)
          ucy(k2) = ucy(k2) + wcy2(L)*u1(L)
       endif
    enddo

    do L = lnx1D + 1,lnx
       if (jabarrieradvection == 3) then
          if ( struclink(L) == 1 ) cycle
       endif
       if (u1(L) .ne. 0d0) then                      ! link flows
          k1 = ln(1,L) ; k2 = ln(2,L)
          ucx(k1) = ucx(k1) + wcx1(L)*u1(L)
          ucy(k1) = ucy(k1) + wcy1(L)*u1(L)
          ucx(k2) = ucx(k2) + wcx2(L)*u1(L)
          ucy(k2) = ucy(k2) + wcy2(L)*u1(L)
       endif
    enddo

 else
    do LL = 1,lnx
       Lb = Lbot(LL) ; Lt = Lb - 1 + kmxL(LL)
       do L = Lb, Lt
          if (u1(L) .ne. 0d0) then                         ! link flows
             k1 = ln0(1,L)                                 ! use ln0 in reconstruction and in computing ucxu, use ln when fluxing
             k2 = ln0(2,L)
             ucx (k1) = ucx (k1) + wcx1(LL)*u1(L)
             ucy (k1) = ucy (k1) + wcy1(LL)*u1(L)
             ucx (k2) = ucx (k2) + wcx2(LL)*u1(L)
             ucy (k2) = ucy (k2) + wcy2(LL)*u1(L)
          endif
       enddo

       if (jazlayercenterbedvel == 1) then ! copy bed velocity down
           do k1 = kbot(ln0(1,LL)), ln0(1,Lb) - 1
              ucx(k1) = ucx(k1) + wcx1(LL)*u1(Lb)
              ucy(k1) = ucy(k1) + wcy1(LL)*u1(Lb)
           enddo
           do k2 = kbot(ln0(2,LL)), ln0(2,Lb) - 1
              ucx(k2) = ucx(k2) + wcx2(LL)*u1(Lb)
              ucy(k2) = ucy(k2) + wcy2(LL)*u1(Lb)
           enddo
       endif

    enddo

 endif
 endif 


 if (icorio == 5) then                                  ! original 2D coding hu weightings

    if (kmx < 1) then

       do L = lnx1D + 1,lnx
          if (u1(L) .ne. 0d0) then                      ! link flows
             k1 = ln(1,L) ; k2 = ln(2,L)
             huL = hu(L)
             if (hhtrshcor > 0) huL = max(huL, hhtrshcor )
             uhu = u1(L)*huL
             ucxq(k1) = ucxq(k1) + wcx1(L)*uhu
             ucyq(k1) = ucyq(k1) + wcy1(L)*uhu
             ucxq(k2) = ucxq(k2) + wcx2(L)*uhu
             ucyq(k2) = ucyq(k2) + wcy2(L)*uhu
          endif
       enddo

    else

       do LL = 1,lnx
          Lb = Lbot(LL) ; Lt = Lb - 1 + kmxL(LL)
          do L = Lb, Lt
             if (u1(L) .ne. 0d0) then                         ! link flows
                k1 = ln0(1,L)                                 ! use ln0 in reconstruction and in computing ucxu, use ln when fluxing
                k2 = ln0(2,L)
                huL = hu(L) - hu(L-1)
                if (hhtrshcor > 0) huL = max(huL, hhtrshcor )
                uhu      = u1(L)*huL
                ucxq(k1) = ucxq(k1) + wcx1(LL)*uhu
                ucyq(k1) = ucyq(k1) + wcy1(LL)*uhu
                ucxq(k2) = ucxq(k2) + wcx2(LL)*uhu
                ucyq(k2) = ucyq(k2) + wcy2(LL)*uhu
             endif
          enddo
       enddo

    endif

 else if (icorio == 6) then                             ! hu2D weighting

    if (kmx < 1) then

       do L = lnx1D + 1,lnx
          if (u1(L) .ne. 0d0) then                      ! link flows
             k1 = ln(1,L) ; k2 = ln(2,L)
             huL = hu(L)
             if (hhtrshcor > 0) huL = max(huL, hhtrshcor )
             uhu = u1(L)*huL
             ucxq(k1) = ucxq(k1) + wcx1(L)*uhu
             ucyq(k1) = ucyq(k1) + wcy1(L)*uhu
             ucxq(k2) = ucxq(k2) + wcx2(L)*uhu
             ucyq(k2) = ucyq(k2) + wcy2(L)*uhu
          endif
       enddo

    else

       do LL = 1,lnx
          Lb = Lbot(LL) ; Lt = Lb - 1 + kmxL(LL)
          huL = hu(LL)
          if (hhtrshcor > 0) huL = max(huL, hhtrshcor )
          do L = Lb, Lt
             if (u1(L) .ne. 0d0) then                         ! link flows
                k1 = ln0(1,L)                                 ! use ln0 in reconstruction and in computing ucxu, use ln when fluxing
                k2 = ln0(2,L)
                uhu      = u1(L)*huL
                ucxq(k1) = ucxq(k1) + wcx1(LL)*uhu
                ucyq(k1) = ucyq(k1) + wcy1(LL)*uhu
                ucxq(k2) = ucxq(k2) + wcx2(LL)*uhu
                ucyq(k2) = ucyq(k2) + wcy2(LL)*uhu
             endif
          enddo
       enddo

    endif

 else if (icorio == 7) then                             ! ahuk type weigthings

   if (kmx < 1) then

       do L = lnx1D + 1,lnx
          if (u1(L) .ne. 0d0) then                      ! link flows
             k1 = ln(1,L) ; k2 = ln(2,L)
             huL = acl(L)*hs(k1) + (1d0-acl(L))*hs(k2)
             if (hhtrshcor > 0) huL = max(huL, hhtrshcor )
             uhu = u1(L)*huL
             ucxq(k1) = ucxq(k1) + wcx1(L)*uhu
             ucyq(k1) = ucyq(k1) + wcy1(L)*uhu
             ucxq(k2) = ucxq(k2) + wcx2(L)*uhu
             ucyq(k2) = ucyq(k2) + wcy2(L)*uhu
          endif
       enddo

    else

       do LL = 1,lnx
          Lb = Lbot(LL) ; Lt = Lb - 1 + kmxL(LL)
          do L = Lb, Lt
             if (u1(L) .ne. 0d0) then                         ! link flows
                k1 = ln0(1,L)                                 ! use ln0 in reconstruction and in computing ucxu, use ln when fluxing
                k2 = ln0(2,L)
                huL = acl(LL)*( zws(k1) - zws(k1-1) )  + (1d0-acl(LL))*( zws(k2) - zws(k2-1) )
                if (hhtrshcor > 0) huL = max(huL, hhtrshcor )
                uhu      = u1(L)*huL
                ucxq(k1) = ucxq(k1) + wcx1(LL)*uhu
                ucyq(k1) = ucyq(k1) + wcy1(LL)*uhu
                ucxq(k2) = ucxq(k2) + wcx2(LL)*uhu
                ucyq(k2) = ucyq(k2) + wcy2(LL)*uhu
             endif
          enddo
       enddo

    endif

else if (icorio == 8) then                             ! ahu2D type weigthings

   if (kmx < 1) then

       do L = lnx1D + 1,lnx
          if (u1(L) .ne. 0d0) then                      ! link flows
             k1 = ln(1,L) ; k2 = ln(2,L)
             huL = acl(L)*hs(k1) + (1d0-acl(L))*hs(k2)
             if (hhtrshcor > 0) huL = max(huL, hhtrshcor )
             uhu = u1(L)*huL
             ucxq(k1) = ucxq(k1) + wcx1(L)*uhu
             ucyq(k1) = ucyq(k1) + wcy1(L)*uhu
             ucxq(k2) = ucxq(k2) + wcx2(L)*uhu
             ucyq(k2) = ucyq(k2) + wcy2(L)*uhu
          endif
       enddo

    else

       do LL = 1,lnx
          Lb = Lbot(LL) ; Lt = Lb - 1 + kmxL(LL)
          k1 = ln(1,LL) ; k2 = ln(2,LL)
          huL = acl(LL)*hs(k1) + (1d0-acl(LL))*hs(k2)
          if (hhtrshcor > 0) huL = max(huL, hhtrshcor )
          do L = Lb, Lt
             if (u1(L) .ne. 0d0) then                         ! link flows
                k1 = ln0(1,L)                                 ! use ln0 in reconstruction and in computing ucxu, use ln when fluxing
                k2 = ln0(2,L)
                uhu      = u1(L)*huL
                ucxq(k1) = ucxq(k1) + wcx1(LL)*uhu
                ucyq(k1) = ucyq(k1) + wcy1(LL)*uhu
                ucxq(k2) = ucxq(k2) + wcx2(LL)*uhu
                ucyq(k2) = ucyq(k2) + wcy2(LL)*uhu
             endif
          enddo
       enddo

    endif

else if (icorio == 9) then                             ! volk type weigthings

   if (kmx < 1) then

       do L = lnx1D + 1,lnx
          if (u1(L) .ne. 0d0) then                      ! link flows
             k1 = ln(1,L) ; k2 = ln(2,L)
             huL = acl(L)*vol1(k1) + (1d0-acl(L))*vol1(k2)
             if (hhtrshcor > 0)  huL = max(huL, hhtrshcor*( acl(L)*ba(k1) + (1d0-acl(L))*ba(k2) ) )
             uhu = u1(L)*huL
             ucxq(k1) = ucxq(k1) + wcx1(L)*uhu
             ucyq(k1) = ucyq(k1) + wcy1(L)*uhu
             ucxq(k2) = ucxq(k2) + wcx2(L)*uhu
             ucyq(k2) = ucyq(k2) + wcy2(L)*uhu
          endif
       enddo

    else

       do LL = 1,lnx
          Lb = Lbot(LL) ; Lt = Lb - 1 + kmxL(LL)
          n1 = ln(1,LL) ; n2 = ln(2,LL)
          if (hhtrshcor > 0) htrs = hhtrshcor*( acl(LL)*ba(n1) + (1d0-acl(LL))*ba(n2) )
          do L = Lb, Lt
             if (u1(L) .ne. 0d0) then                         ! link flows
                k1 = ln0(1,L)                                 ! use ln0 in reconstruction and in computing ucxu, use ln when fluxing
                k2 = ln0(2,L)
                huL = acl(LL)*vol1(k1) + (1d0-acl(LL))*vol1(k2)
                if (hhtrshcor > 0)  huL = max(huL, htrs  )
                uhu      = u1(L)*huL
                ucxq(k1) = ucxq(k1) + wcx1(LL)*uhu
                ucyq(k1) = ucyq(k1) + wcy1(LL)*uhu
                ucxq(k2) = ucxq(k2) + wcx2(LL)*uhu
                ucyq(k2) = ucyq(k2) + wcy2(LL)*uhu
             endif
          enddo
       enddo

    endif

else if (icorio == 10) then                             ! vol2D type weigthings

   if (kmx < 1) then

       do L = lnx1D + 1,lnx
          if (u1(L) .ne. 0d0) then                      ! link flows
             k1 = ln(1,L) ; k2 = ln(2,L)
             huL = acl(L)*vol1(k1) + (1d0-acl(L))*vol1(k2)
             if (hhtrshcor > 0)  huL = max(huL, hhtrshcor*( acl(L)*ba(k1) + (1d0-acl(L))*ba(k2) ) )
             uhu = u1(L)*huL
             ucxq(k1) = ucxq(k1) + wcx1(L)*uhu
             ucyq(k1) = ucyq(k1) + wcy1(L)*uhu
             ucxq(k2) = ucxq(k2) + wcx2(L)*uhu
             ucyq(k2) = ucyq(k2) + wcy2(L)*uhu
          endif
       enddo

    else

       do LL = 1,lnx
          Lb = Lbot(LL) ; Lt = Lb - 1 + kmxL(LL)
          k1 = ln(1,LL) ; k2 = ln(2,LL)
          huL = acl(LL)*vol1(k1) + (1d0-acl(LL))*vol1(k2)
          if (hhtrshcor > 0)  huL = max(huL, hhtrshcor*( acl(LL)*ba(k1) + (1d0-acl(LL))*ba(k2) ) )
          do L = Lb, Lt
             if (u1(L) .ne. 0d0) then                         ! link flows
                k1 = ln0(1,L)                                 ! use ln0 in reconstruction and in computing ucxu, use ln when fluxing
                k2 = ln0(2,L)
                uhu      = u1(L)*huL
                ucxq(k1) = ucxq(k1) + wcx1(LL)*uhu
                ucyq(k1) = ucyq(k1) + wcy1(LL)*uhu
                ucxq(k2) = ucxq(k2) + wcx2(LL)*uhu
                ucyq(k2) = ucyq(k2) + wcy2(LL)*uhu
             endif
          enddo
       enddo

    endif


 endif

 if (icorio == 7 .or. icorio == 27) then   ! make ahus or ahusk
    hus = 0
    if (kmx < 1) then ! original 2D coding
       do L = 1,lnx
          k1  = ln(1,L) ; k2 = ln(2,L)
          huL = acl(L)*hs(k1) + (1d0-acl(L))*hs(k2)
          hus(k1) = hus(k1) + wcl(1,L)*huL
          hus(k2) = hus(k2) + wcl(2,L)*huL
       enddo
    else
       do LL = 1,lnx
          do L = Lbot(LL), Ltop(LL)
             k1  = ln(1,L) ; k2 = ln(2,L)
             huL = acl(LL)*( zws(k1) - zws(k1-1) )  + (1d0-acl(LL))*( zws(k2) - zws(k2-1) )
             hus(k1) = hus(k1) + wcl(1,LL)*huL
             hus(k2) = hus(k2) + wcl(2,LL)*huL
          enddo
       enddo
    endif
 else if (icorio == 8 .or. icorio == 28) then
    hus = 0
    if (kmx < 1) then ! original 2D coding
       do L = 1,lnx
          k1  = ln(1,L) ; k2 = ln(2,L)
          huL = acl(L)*hs(k1) + (1d0-acl(L))*hs(k2)
          hus(k1) = hus(k1) + wcl(1,L)*huL
          hus(k2) = hus(k2) + wcl(2,L)*huL
       enddo
    else
       do LL = 1,lnx
          k1  = ln(1,LL) ; k2 = ln(2,LL)
          huL = acl(LL)*hs(k1) + (1d0-acl(LL))*hs(k2)
          do L = Lbot(LL), Ltop(LL)
             hus(k1) = hus(k1) + wcl(1,LL)*huL
             hus(k2) = hus(k2) + wcl(2,LL)*huL
          enddo
       enddo
    endif
 endif


 if (icorio == 5) then                       ! original hu/hs weighting
    if (kmx < 1) then
       !$OMP PARALLEL DO           &
       !$OMP PRIVATE(k,hsk)
       do k = 1,ndxi
          hsk = hs(k)
          if (hsk > 0d0)  then
             if (hhtrshcor > 0)  hsk = max(hsk, hhtrshcor)
             ucxq(k) = ucxq(k)/hsk
             ucyq(k) = ucyq(k)/hsk
          endif
       enddo
       !$OMP END PARALLEL DO
    else
       do nn = 1,ndxi
          if (hs(nn) > 0d0)  then
             kb = kbot(nn)
             kt = ktop(nn)
             do k = kb,kt
                hsk = zws(k) - zws(k-1)
                if (hsk > 0d0) then
                   if (hhtrshcor > 0)  hsk = max(hsk, hhtrshcor)
                   ucxq(k) = ucxq(k)/hsk
                   ucyq(k) = ucyq(k)/hsk
                endif
             enddo
          endif
       enddo
    endif

 else if (icorio == 6) then                       ! original hu/hs weighting 2D
    if (kmx < 1) then
       !$OMP PARALLEL DO           &
       !$OMP PRIVATE(k,hsk)
       do k = 1,ndxi
          hsk = hs(k)
          if (hsk > 0d0)  then
             if (hhtrshcor > 0)  hsk = max(hsk, hhtrshcor)
             ucxq(k) = ucxq(k)/hsk
             ucyq(k) = ucyq(k)/hsk
          endif
       enddo
       !$OMP END PARALLEL DO
    else
       do nn = 1,ndxi
          hsk = hs(nn)
          if (hsk > 0d0)  then
             if (hhtrshcor > 0)  hsk = max(hsk, hhtrshcor)
             kb = kbot(nn)
             kt = ktop(nn)
             do k = kb,kt
                ucxq(k) = ucxq(k)/hsk
                ucyq(k) = ucyq(k)/hsk
             enddo
          endif
       enddo
    endif

 else if (icorio == 7) then                  !          ahuk/ahusk weighting

    if (kmx < 1) then ! original 2D coding
       !$OMP PARALLEL DO           &
       !$OMP PRIVATE(k,hsk)
       do k = 1,ndxi
          if (hus(k) > 0d0)  then
             hsk = hus(k)
             if (hhtrshcor > 0) hsk = max(hsk, hhtrshcor)
             ucxq(k) = ucxq(k)/hsk
             ucyq(k) = ucyq(k)/hsk
          endif
       enddo
       !$OMP END PARALLEL DO
    else
       do nn = 1,ndxi
          if (hs(nn) > 0d0)  then
             kb = kbot(nn)
             kt = ktop(nn)
             do k = kb,kt
                hsk = hus(k)
                if (hsk > 0d0) then
                   if (hhtrshcor > 0) hsk = max(hsk, hhtrshcor)
                   ucxq(k) = ucxq(k)/hsk
                   ucyq(k) = ucyq(k)/hsk
                endif
             enddo
          endif
       enddo
    endif

 else if (icorio == 8) then                  !          ahu/ahus weighting

    if (kmx < 1) then ! original 2D coding
       !$OMP PARALLEL DO           &
       !$OMP PRIVATE(k,hsk)
       do k = 1,ndxi
          if (hus(k) > 0d0)  then
             hsk = hus(k)
             if (hhtrshcor > 0) hsk = max(hsk, hhtrshcor)
             ucxq(k) = ucxq(k)/hsk
             ucyq(k) = ucyq(k)/hsk
          endif
       enddo
       !$OMP END PARALLEL DO
    else
       do nn = 1,ndxi
          if (hs(nn) > 0d0)  then
             kb = kbot(nn)
             kt = ktop(nn)
             hsk = hus(nn)
             if (hsk > 0) then
                if (hhtrshcor > 0) hsk = max(hsk, hhtrshcor)
                do k = kb,kt
                   ucxq(k) = ucxq(k)/hsk
                   ucyq(k) = ucyq(k)/hsk
                enddo
             endif
          endif
       enddo
    endif

 else if (icorio == 9) then                  !          voluk/volk weighting

    if (kmx < 1) then ! original 2D coding
       !$OMP PARALLEL DO           &
       !$OMP PRIVATE(k,hsk)
       do k = 1,ndxi
          if (vol1(k) > 0d0)  then
             hsk = vol1(k)
             if (hhtrshcor > 0) hsk = max(hsk, hhtrshcor*ba(k))
             ucxq(k) = ucxq(k)/hsk
             ucyq(k) = ucyq(k)/hsk
          endif
       enddo
       !$OMP END PARALLEL DO
    else
       do nn = 1,ndxi
          if (vol1(nn) > 0d0)  then
             kb = kbot(nn)
             kt = ktop(nn)
             do k = kb,kt
                hsk = vol1(k)
                if (hsk > 0d0) then
                   if (hhtrshcor > 0) hsk = max(hsk, hhtrshcor*ba(nn))
                   ucxq(k) = ucxq(k)/hsk
                   ucyq(k) = ucyq(k)/hsk
                endif
             enddo
          endif
       enddo
    endif
 else if (icorio == 10) then                  !          volu/vol weighting

    if (kmx < 1) then ! original 2D coding
       !$OMP PARALLEL DO           &
       !$OMP PRIVATE(k,hsk)
       do k = 1,ndxi
          if (vol1(k) > 0d0)  then
             hsk = vol1(k)
             if (hhtrshcor > 0) hsk = max(hsk, hhtrshcor*ba(k))
             ucxq(k) = ucxq(k)/hsk
             ucyq(k) = ucyq(k)/hsk
          endif
       enddo
       !$OMP END PARALLEL DO
    else
       do nn = 1,ndxi
          if (vol1(nn) > 0d0)  then
             kb = kbot(nn)
             kt = ktop(nn)
             hsk = vol1(nn)
             if (hsk > 0) then
                if (hhtrshcor > 0) hsk = max(hsk, hhtrshcor*ba(nn))
                do k = kb,kt
                   ucxq(k) = ucxq(k)/hsk
                   ucyq(k) = ucyq(k)/hsk
                enddo
             endif
          endif
       enddo
    endif

 else if (icorio > 0) then ! all Ham types
    ucxq = ucx
    ucyq = ucy
 endif

 if (icorio > 0) then  ! and no more touching after this
    do LL = Lnxi+1,Lnx
       do L = lbot(LL), Ltop(LL)
          k1 = ln(1,L) ; k2 = ln(2,L)
          ucxq(k1) = ucxq(k2)
          ucyq(k1) = ucyq(k2)
       enddo
    enddo
 endif

 do n  = 1, nbndz                                     ! waterlevel boundaries
    kb = kbndz(1,n)
    k2 = kbndz(2,n)
    LL = kbndz(3,n)
    itpbn = kbndz(4,n)
    cs = csu(LL) ; sn = snu(LL)
    if (kmx == 0) then
       if (hs(kb) > epshs)  then
          if ( jacstbnd.eq.0 .and. itpbn.ne.2 ) then    ! Neumann: always
             if (jasfer3D == 1) then
                uin     = nod2linx(LL,2,ucx(k2),ucy(k2))*cs + nod2liny(LL,2,ucx(k2),ucy(k2))*sn
                ucx(kb) = uin*lin2nodx(LL,1,cs,sn)
                ucy(kb) = uin*lin2nody(LL,1,cs,sn)
             else
                uin     = ucx(k2)*cs + ucy(k2)*sn
                ucx(kb) = uin*cs
                ucy(kb) = uin*sn
             end if
          else
             if (jasfer3D == 1) then
                uinx    = nod2linx(LL,2,ucx(k2),ucy(k2))
                uiny    = nod2liny(LL,2,ucx(k2),ucy(k2))
                ucx(kb) = lin2nodx(LL,1,uinx,uiny)
                ucy(kb) = lin2nody(LL,1,uinx,uiny)
             else
                ucx(kb) = ucx(k2)
                ucy(kb) = ucy(k2)
             endif
          end if
       endif
    else
       call getLbotLtop(LL,Lb,Lt)
       do L = Lb, Lt
          kbk = ln(1,L) ; k2k = ln(2,L)
          if ( jacstbnd.eq.0 .and. itpbn.ne.2 ) then
             if (jasfer3D == 1) then
                uin = nod2linx(LL,2,ucx(k2k),ucy(k2k))*cs + nod2liny(LL,2,ucx(k2k),ucy(k2k))*sn
                ucx(kbk) = uin*lin2nodx(LL,1,cs,sn)
                ucy(kbk) = uin*lin2nody(LL,1,cs,sn)
             else
                uin = ucx(k2k)*cs + ucy(k2k)*sn
                ucx(kbk) = uin*cs
                ucy(kbk) = uin*sn
             end if
          else
             if (jasfer3D == 1) then
                uinx     = nod2linx(LL,2,ucx(k2k),ucy(k2k))
                uiny     = nod2liny(LL,2,ucx(k2k),ucy(k2k))
                ucx(kbk) = lin2nodx(LL,1,uinx,uiny)
                ucy(kbk) = lin2nody(LL,1,uinx,uiny)
             else
                ucx(kbk) = ucx(k2k)
                ucy(kbk) = ucy(k2k)
             end if
          end if
       enddo
    endif
 enddo

 if (jaZerozbndinflowadvection == 1) then
    do n  = 1, nbndz                                      ! on waterlevel boundaries put inflow advection velocity to 0 on inflow
       LL = kbndz(3,n)
       do L  = Lbot(LL), Ltop(LL)
          k1 = ln(1,L)
          if (u1(LL) > 0) then
             ucx(k1) = 0d0 ; ucy(k1) = 0d0
          endif
       enddo
    enddo
 else if (jaZerozbndinflowadvection == 2) then
    do n  = 1, nbndz                                      ! on waterlevel boundaries put all advection velocity to 0 anyway
       LL = kbndz(3,n)
       do L  = Lbot(LL), Ltop(LL)
          k1 = ln(1,L)
          ucx(k1) = 0d0 ; ucy(k1) = 0d0
       enddo
    enddo
 endif

 do n  = 1,nbndu                                          ! velocity boundaries
    kb = kbndu(1,n)
    k2 = kbndu(2,n)
    LL = kbndu(3,n)
    cs = csu(LL) ; sn = snu(LL)
    if (kmx == 0) then
       if (hs(kb) > epshs)  then
          if ( jacstbnd.eq.0 ) then
             if (jasfer3D == 1) then
                uin     = nod2linx(LL,2,ucx(k2),ucy(k2))*cs + nod2liny(LL,2,ucx(k2),ucy(k2))*sn
                ucx(kb) = uin*lin2nodx(LL,1,cs,sn)
                ucy(kb) = uin*lin2nody(LL,1,cs,sn)
             else
                uin     = ucx(k2)*cs + ucy(k2)*sn
                ucx(kb) = uin*cs
                ucy(kb) = uin*sn
             end if
          else
             if (jasfer3D == 1) then
                uinx    = nod2linx(LL,2,ucx(k2),ucy(k2))
                uiny    = nod2liny(LL,2,ucx(k2),ucy(k2))
                ucx(kb) = lin2nodx(LL,1,uinx,uiny)
                ucy(kb) = lin2nody(LL,1,uinx,uiny)
             else
                ucx(kb) = ucx(k2)
                ucy(kb) = ucy(k2)
             end if
          end if
       endif
    else
       do k   = 1, kmxL(LL)
          kbk = kbot(kb) - 1 + min(k,kmxn(kb))
          k2k = kbot(k2) - 1 + min(k,kmxn(k2))
          if ( jacstbnd.eq.0 ) then
             if (jasfer3D == 1) then
                uin      = nod2linx(LL,2,ucx(k2k),ucy(k2k))*cs + nod2liny(LL,2,ucx(k2k),ucy(k2k))*sn
                ucx(kbk) = uin*lin2nodx(LL,1,cs,sn)
                ucy(kbk) = uin*lin2nody(LL,1,cs,sn)
             else
                uin = ucx(k2k)*cs + ucy(k2k)*sn
                ucx(kbk) = uin*cs
                ucy(kbk) = uin*sn
             end if
          else
             if (jasfer3D == 1) then
                uinx     = nod2linx(LL,2,ucx(k2k),ucy(k2k))
                uiny     = nod2liny(LL,2,ucx(k2k),ucy(k2k))
                ucx(kbk) = lin2nodx(LL,1,uinx,uiny)
                ucy(kbk) = lin2nody(LL,1,uinx,uiny)
             else
                ucx(kbk) = ucx(k2k)
                ucy(kbk) = ucy(k2k)
             end if
          end if
       enddo
    endif
 enddo

 do n  = 1, nbndt                               ! tangential velocity boundaries, override other types
    kb = kbndt(1,n)
    k2 = kbndt(2,n)
    LL = kbndt(3,n)
    cs = csu(LL) ; sn = snu(LL)
    call getLbotLtop(LL,Lb,Lt)
    do L = Lb, Lt
       kbk  = ln(1,L)
       kk   = kmxd*(n-1)+L-Lb+1
       uu   = u0(L) ; vv = zbndt(kk) ! v(L)
       uucx = uu*cs - vv*sn
       uucy = uu*sn + vv*cs
       if (jasfer3D == 1) then
          ucx(kbk) = lin2nodx(LL,1,uucx,uucy)
          ucy(kbk) = lin2nody(LL,1,uucx,uucy)
       else
          ucx(kbk) = uucx
          ucy(kbk) = uucy
       end if
    enddo
 enddo

 if (zbnduxyval .ne. dmiss) then
     zbnduxy(1) = zbnduxyval
 endif

 do n  = 1, nbnduxy        ! do3d                     ! uxuy velocity boundaries, override other types
    LL = kbnduxy(3,n)
    call getLbotLtop(LL,Lb,Lt)
    do L = Lb, Lt
       kbk = ln(1,L)
       kk  = kmxd*(n-1)+L-Lb+1
       if (jasfer3D == 1) then
          ucx(kbk) = lin2nodx(LL,1,zbnduxy(2*kk-1),zbnduxy(2*kk))
          ucy(kbk) = lin2nody(LL,1,zbnduxy(2*kk-1),zbnduxy(2*kk))
       else
          ucx(kbk) = zbnduxy(2*kk-1)
          ucy(kbk) = zbnduxy(2*kk)
       end if
       if (jazerozbndinflowadvection == 3) then  !
          k2 = ln(2,L)
          ucx(k2) = 0.5d0*( ucx(kbk) + ucx(k2) )
          ucy(k2) = 0.5d0*( ucy(kbk) + ucy(k2) )
       endif
    enddo
 enddo

 do n  = 1, nbndn                                     ! normal velocity boundaries, override other types
    kb = kbndn(1,n)
    k2 = kbndn(2,n)
    LL = kbndn(3,n)
    cs = csu(LL) ; sn = snu(LL)
    call getLbotLtop(LL,Lb,Lt)
    do L = Lb, Lt
       kbk  = ln(1,L)
       kk   = kmxd*(n-1)+L-Lb+1
       uu   = zbndn(kk) ; vv = 0d0
       uucx = uu*cs - vv*sn                   !
       uucy = uu*sn + vv*cs
       if (jasfer3D == 1) then
          ucx(kbk) = lin2nodx(LL,1,uucx,uucy)
          ucy(kbk) = lin2nody(LL,1,uucx,uucy)
       else
          ucx(kbk) = uucx
          ucy(kbk) = uucy
       end if
    enddo
 enddo

 do n=1,nbnd1d2d
    kb      = kbnd1d2d(1,n)
    k2      = kbnd1d2d(2,n)
    LL      = kbnd1d2d(3,n)

    if (kmx == 0) then     ! 2D
       if (jasfer3D == 1) then
          uinx = nod2linx(LL,2,ucx(k2),ucy(k2))
          uiny = nod2liny(LL,2,ucx(k2),ucy(k2))
          ucx(kb) = lin2nodx(LL,1,uinx,uiny)
          ucy(kb) = lin2nody(LL,1,uinx,uiny)
       else
          ucx(kb) = ucx(k2)
          ucy(kb) = ucy(k2)
       end if
    else     ! 3D

    endif
 end do

 if (limtypmom == 6) then

    ducxdx = 0d0; ducxdy = 0d0
    ducydx = 0d0; ducydy = 0d0
    do LL = 1,lnx
       Lb = Lbot(LL) ; Lt = Lb - 1 + kmxL(LL)
       do L = Lb, Lt
          k1 = ln(1,L)
          k2 = ln(2,L)
          if (jasfer3D == 1) then
             duxdn = dxi(LL)*(nod2linx(LL,2,ucx(k2),ucy(k2))-nod2linx(LL,1,ucx(k1),ucy(k1)))
          else
             duxdn = dxi(LL)*(ucx(k2) - ucx(k1))
          endif
          ducxdx(k1) = ducxdx(k1) + wcx1(LL)*duxdn
          ducxdy(k1) = ducxdy(k1) + wcy1(LL)*duxdn
          ducxdx(k2) = ducxdx(k2) + wcx2(LL)*duxdn
          ducxdy(k2) = ducxdy(k2) + wcy2(LL)*duxdn

          if (jasfer3D == 1) then
             duydn = dxi(LL)*(nod2liny(LL,2,ucx(k2),ucy(k2))-nod2liny(LL,1,ucx(k1),ucy(k1)))
          else
             duydn = dxi(LL)*(ucy(k2) - ucy(k1))
          endif
          ducydx(k1) = ducydx(k1) + wcx1(LL)*duydn
          ducydy(k1) = ducydy(k1) + wcy1(LL)*duydn
          ducydx(k2) = ducydx(k2) + wcx2(LL)*duydn
          ducydy(k2) = ducydy(k2) + wcy2(LL)*duydn
       enddo
    enddo

    !do nw  = 1,mxwalls   ! to be finished later zz
    !   csw = walls(7,nw)
    !   snw = walls(8,nw)
    !   ducdn = 2d0*(ucx(k1)*csw +
    !   ducxdx(k1) = ducxdx(k1) + *duxdn
    !enddo

 endif

 if (kmx < 1) then

    if (jasfer3D == 1) then
       !$OMP PARALLEL DO           &
       !$OMP PRIVATE(L)
       do L = 1,lnx
          if (qa(L) > 0) then                               ! set upwind ucxu, ucyu  on links
             ucxu(L) = nod2linx(L,1,ucx(ln(1,L)),ucy(ln(1,L)))
             ucyu(L) = nod2liny(L,1,ucx(ln(1,L)),ucy(ln(1,L)))
          else if (qa(L) < 0) then
             ucxu(L) = nod2linx(L,2,ucx(ln(2,L)),ucy(ln(2,L)))
             ucyu(L) = nod2liny(L,2,ucx(ln(2,L)),ucy(ln(2,L)))
          else
             ucxu(L) = 0d0
             ucyu(L) = 0d0
          endif
       enddo
       !$OMP END PARALLEL DO
    else
       !$OMP PARALLEL DO           &
       !$OMP PRIVATE(L)
       do L = 1,lnx
          if (qa(L) > 0) then                               ! set upwind ucxu, ucyu  on links
             ucxu(L) = ucx(ln(1,L))
             ucyu(L) = ucy(ln(1,L))
          else if (qa(L) < 0) then
             ucxu(L) = ucx(ln(2,L))
             ucyu(L) = ucy(ln(2,L))
          else
             ucxu(L) = 0d0
             ucyu(L) = 0d0
          endif
       enddo
       !$OMP END PARALLEL DO
    endif

 else

    if (jasfer3D == 1) then
       !$OMP PARALLEL DO           &
       !$OMP PRIVATE(LL,L,Lb,Lt)
       do LL = 1,lnx
          call getLbotLtop(LL,Lb,Lt)
          do L = Lb,Lt
             if (qa(L) > 0) then                               ! set upwind ucxu, ucyu  on links
                ucxu(L) = nod2linx(LL,1,ucx(ln0(1,L)),ucy(ln0(1,L)))
                ucyu(L) = nod2liny(LL,1,ucx(ln0(1,L)),ucy(ln0(1,L)))
                if (jarhoxu > 0) then
                   ucxu(L) = ucxu(L)*rho(ln(1,L))
                   ucyu(L) = ucyu(L)*rho(ln(1,L))
                endif
             else if (qa(L) < 0) then
                ucxu(L) = nod2linx(LL,2,ucx(ln0(2,L)),ucy(ln0(2,L)))
                ucyu(L) = nod2liny(LL,2,ucx(ln0(2,L)),ucy(ln0(2,L)))
                if (jarhoxu > 0) then
                   ucxu(L) = ucxu(L)*rho(ln(2,L))
                   ucyu(L) = ucyu(L)*rho(ln(2,L))
                endif
             else
                ucxu(L) = 0d0
                ucyu(L) = 0d0
             endif
          enddo
       enddo
       !$OMP END PARALLEL DO

    else

       !$OMP PARALLEL DO           &
       !$OMP PRIVATE(LL,L,Lb,Lt)
       do LL = 1,lnx
          call getLbotLtop(LL,Lb,Lt)
          do L = Lb,Lt
             if (qa(L) > 0) then                               ! set upwind ucxu, ucyu  on links
                ucxu(L) = ucx(ln0(1,L))
                ucyu(L) = ucy(ln0(1,L))
                if (jarhoxu > 0) then
                   ucxu(L) = ucxu(L)*rho(ln(1,L))
                   ucyu(L) = ucyu(L)*rho(ln(1,L))
                endif
             else if (qa(L) < 0) then
                ucxu(L) = ucx(ln0(2,L))
                ucyu(L) = ucy(ln0(2,L))
                if (jarhoxu > 0) then
                   ucxu(L) = ucxu(L)*rho(ln(2,L))
                   ucyu(L) = ucyu(L)*rho(ln(2,L))
                endif
             else
                ucxu(L) = 0d0
                ucyu(L) = 0d0
             endif
          enddo
       enddo
       !$OMP END PARALLEL DO

    endif

 endif

 if (kmx == 0 .and. lnx1D > 0 ) then ! setuc
    call setuc1D ()
 endif

end subroutine setucxucyucxuucyunew
