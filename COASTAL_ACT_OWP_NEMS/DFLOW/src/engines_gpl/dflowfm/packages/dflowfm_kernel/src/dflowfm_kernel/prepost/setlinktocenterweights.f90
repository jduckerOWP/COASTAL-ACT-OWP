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

! $Id: setlinktocenterweights.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/setlinktocenterweights.f90 $

 subroutine setlinktocenterweights()                 ! set center related linkxy weights

 use m_flow
 use m_netw
 use m_flowgeom
 use m_sferic

 implicit none

 double precision       :: wud, wuL1, wuL2, wuk, cs, sn
 integer                :: k, L, ierr, n, kk, n12, lnxmax
 integer                :: k1, k2, k3, k4, nn, LL, jaclosedcorner

 double precision       :: xloc, yloc, beta, aa1, wcw, alf
 double precision, allocatable       :: wwL(:)

 double precision, allocatable       :: wcxy (:,:)   ! center weight factors (2,ndx) , only for normalising
 double precision, allocatable       :: wc   (:)     ! center weight factors (ndx)   , only for normalising

 double precision, external :: lin2nodx, lin2nody

 if ( allocated (wcx1) )  deallocate(wcx1,wcy1,wcx2,wcy2)
 if ( allocated (wcxy ) ) deallocate(wcxy )
 if ( allocated (wcL  ) ) deallocate(wcL )

 allocate ( wcx1(lnx) , stat  = ierr) ; wcx1 = 0
 call aerr('wcx1(lnx)', ierr, lnx)
 allocate ( wcy1(lnx) , stat  = ierr) ; wcy1 = 0
 call aerr('wcy1(lnx)', ierr, lnx)
 allocate ( wcx2(lnx) , stat  = ierr) ; wcx2 = 0
 call aerr('wcx2(lnx)', ierr, lnx)
 allocate ( wcy2(lnx) , stat  = ierr) ; wcy2 = 0
 call aerr('wcy2(lnx)', ierr, lnx)
 allocate ( wcxy (2,ndx) , stat  = ierr) ; wcxy  = 0
 call aerr('wcxy (2,ndx)', ierr, 2*ndx)
 allocate ( wcL  (2,Lnx) , stat  = ierr) ; wcL   = 0
 call aerr('wcL  (2,Lnx)', ierr, 2*Lnx)
 allocate ( wc     (ndx) , stat  = ierr) ; wc    = 0
 call aerr('wc     (ndx)', ierr, ndx)

 do L = 1, lnx

    if (kcu(L) == 3) cycle ! no contribution from 1D2D internal links

    k1   = ln(1,L) ; k2 = ln(2,L)
    wud  = wu(L)*dx(L)
!    cs   = csu(L)
!    sn   = snu(L)

    wuL1  = acl(L)*wud
    wcL  (1,L ) = wuL1
    wc     (k1) = wc(k1) + wuL1

    wuL2  = (1d0-acl(L))*wud
    wcL  (2, L) = wuL2
    wc     (k2) = wc(k2) + wuL2

    cs = lin2nodx(L,1,csu(L),snu(L))
    sn = lin2nody(L,1,csu(L),snu(L))
    wcx1(L)     = cs*wuL1
    wcy1(L)     = sn*wuL1

    cs = lin2nodx(L,2,csu(L),snu(L))
    sn = lin2nody(L,2,csu(L),snu(L))
    wcx2(L)     = cs*wuL2
    wcy2(L)     = sn*wuL2

    wcxy (1,k1) = wcxy (1,k1) + abs(wcx1(L))
    wcxy (2,k1) = wcxy (2,k1) + abs(wcy1(L))

    wcxy (1,k2) = wcxy (1,k2) + abs(wcx2(L))
    wcxy (2,k2) = wcxy (2,k2) + abs(wcy2(L))
 enddo

 lnxmax = 0
 do n   = 1, mxwalls                                        ! wall contribution to scalar linktocenterweights
    k1  = walls(1,n)
    aa1 = 2d0*walls(17,n)
    wcw = 0d0
    lnxmax = max(lnxmax,  nd(k1)%lnx)
    call realloc(wwL, lnxmax, keepExisting = .false.)
    do kk = 1,size(nd(k1)%ln)
       LL = iabs(nd(k1)%ln(kk))
       n12 = 1 ; alf = acL(LL)
       if (k1 .ne. ln(1,LL) ) then
          n12 = 2 ; alf = 1d0-acL(LL)
       endif
       wuL1    =  alf*dx(LL)*wu(LL)
       cs      =  walls(8,n) ! outward positive
       sn      = -walls(7,n)
       wwL(kk) = abs(cs*csu(LL) + sn*snu(LL))
       wwL(kk) = wwL(kk)*wuL1
       wcw     = wcw + wwL(kk)
    enddo
    if (wcw > 0d0) then
       wc(k1) = wc(k1) + aa1
       do kk = 1,size(nd(k1)%ln)
          LL = iabs(nd(k1)%ln(kk))
          n12 = 1 ; alf = acL(LL)
          if (k1 .ne. ln(1,LL) ) then
             n12 = 2 ; alf = 1d0-acL(LL)
          endif
          wcL(n12,LL) = wcL(n12,LL) + wwL(kk)*aa1/wcw
       enddo
    endif
 enddo

 do L = 1, lnx
    k1 = ln(1,L) ; k2 = ln(2,L)
    if (iabs(kcu(L)) == 2 .or. iabs(kcu(L)) == 4) then      ! 2D links and 1D2D lateral links
       IF (kfs(K1) == 0) THEN                               ! kfs temporarily used as cutcell flag, set in cutcelwu
          wcx1(L) = wcx1(L)*bai(k1)
          wcy1(L) = wcy1(L)*bai(k1)
       ELSE
          if (wcxy(1,k1) .ne. 0) wcx1(L) = wcx1(L)/wcxy(1,k1)
          if (wcxy(2,k1) .ne. 0) wcy1(L) = wcy1(L)/wcxy(2,k1)
       ENDIF

       IF (kfs(K2) == 0) THEN
          wcx2(L) = wcx2(L)*bai(k2)
          wcy2(L) = wcy2(L)*bai(k2)
       ELSE
          if (wcxy(1,k2) .ne. 0) wcx2(L) = wcx2(L)/wcxy(1,k2)
          if (wcxy(2,k2) .ne. 0) wcy2(L) = wcy2(L)/wcxy(2,k2)
       ENDIF
    else
       wcx1(L) = wcx1(L)*bai(k1) !if (wcxy(2,k1) .ne. 0) /wcxy(2,k1)
       wcy1(L) = wcy1(L)*bai(k1) !if (wcxy(1,k1) .ne. 0) /wcxy(1,k1)
       wcx2(L) = wcx2(L)*bai(k2) !if (wcxy(2,k2) .ne. 0) /wcxy(2,k2)
       wcy2(L) = wcy2(L)*bai(k2) !if (wcxy(1,k2) .ne. 0) /wcxy(1,k2)
    endif
    if (wc(k1) > 0d0) wcL(1,L) = wcL(1,L) / wc(k1)
    if (wc(k2) > 0d0) wcL(2,L) = wcL(2,L) / wc(k2)

 enddo

 deallocate (wcxy, wc)
 if (allocated(wwL)) deallocate(wwL)

 kfs = 0

 end subroutine setlinktocenterweights
