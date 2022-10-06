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

! $Id: inifcori.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_data/inifcori.f90 $

subroutine inifcori()
 use m_flowgeom
 use m_flow
 use m_sferic
 use network_data
 use messagehandling
 implicit none
 integer          :: ierr, L, k, i, LL, LLL, LLLL, k1, k2, k3, n, j, mout
 double precision :: beta, y0, dy, fcormin, fcormax, xx, yy

 if (jsferic > 0 .or. jacorioconstant .ge. 1) then
    if (allocated(fcori) ) then
       deallocate(fcori)
    endif

    if (icorio <= 6) then
       allocate ( fcori(lnx), stat = ierr )
       call aerr('fcori(lnx)', ierr, lnx  )
       do L = 1,lnx
          fcori(L) = 2d0*omega*sin(yu(L)*dg2rd)
       enddo
       if (jacorioconstant == 2) then ! beta plane)
          if (jsferic == 0) then 
             y0 = anglat
          else
             y0 = 0.5d0*(ykmin + ykmax)
          endif
          beta = ( cos(y0*dg2rd) ) / Ra
          do L = 1,lnx
             call dbdistancehk( xu(L), y0, xu(L), yu(L), dy) ; if (yu(L) < y0) dy = -dy 
             fcori(L) = 2d0*omega*( sin(anglat*dg2rd) + beta*dy ) 
          enddo
       endif

    else
       allocate ( fcori(ndx), stat = ierr )
       call aerr('fcori(ndx)', ierr, ndx  )
       do k = 1,ndx
          fcori(k) = 2d0*omega*sin(yz(k)*dg2rd)
       enddo
       if (jacorioconstant == 2) then ! beta plane)
          if (jsferic == 0) then 
             y0 = anglat
          else
             y0 = 0.5d0*(ykmin + ykmax)
          endif
          beta = ( cos(y0*dg2rd) ) / Ra
          do k = 1,ndx
             call dbdistancehk( xz(k), y0, xz(k), yz(k), dy) ; if (yu(L) < y0) dy = -dy 
             fcori(k) = 2d0*omega*( sin(anglat*dg2rd) + beta*dy ) 
          enddo
       endif
    endif

    if (jacorioconstant == 1) then
       fcori = 2d0*omega*sin(anglat*dg2rd)
    endif
 
 endif

 if ( jasecflow > 0 .and. kmx == 0 ) then

    ! Coriolis in flow node, added by Nabi
     if (allocated(fcoris) ) then
        deallocate(fcoris)
     endif
     allocate ( fcoris(ndx), stat = ierr )
     call aerr('fcoris(ndx)', ierr, ndx  )
     do k = 1,ndx
        fcoris(k) = 2d0*omega*sin(yz(k)*dg2rd)
     enddo
 endif

 if (icorio > 40) then
    !if (allocated(LLkkk) ) then
    !   deallocate(LLkkk)
    !endif

    n = 0
    do j = 1,2  ! 1=count, 2=allocate and use
       if (j == 2) then
           allocate ( LLkkk(5,n) , stat = ierr ); LLkkk = 0
       endif
       n = 0
       do L = 1,lnx
          do i = 1,2
             k = ln(i,L)
             do LL   = 1, nd(k)%lnx                            ! loop over all attached links  k1,L1,k2,L2,k3
                LLL  = nd(k)%ln(LL)                            !                              ( 3  1  4  2  5, L)
                LLLL = iabs(LLL)
                if (L < LLLL) then
                   n = n + 1
                   if (j == 2) then
                      if (i == 2) then
                          k1 = ln(1,L)
                          k2 = ln(2,L)
                      else
                          k1 = ln(2,L)
                          k2 = ln(1,L)
                      endif
                      k3 = ln(1,LLLL) + ln(2,LLLL) - k2
                      LLkkk(1,n) = L
                      LLkkk(2,n) = LLLL
                      LLkkk(3,n) = k1
                      LLkkk(4,n) = k2
                      LLkkk(5,n) = k3
                   endif
                endif
             enddo
          enddo
       enddo
    enddo
 endif

 k = size(fcori)
 if (k > 0) then 
    !call newfil(msgbu, trim(getoutputdir())//trim(md_ident)//'_Cdwcoeff.tek')
    !call newfil(mout,'fcori.xyz')
    fcormin =1d9 ; fcormax = - fcormin
    do i = 1, k
       fcormin = min(fcormin,fcori(i))
       fcormax = max(fcormax,fcori(i))
       if (icorio <= 6) then 
          xx = xu(i); yy = yu(i)
       else
          xx = xz(i); yy = yz(i)
       endif
       !write(mout,*) xx,yy,fcori(i)
    enddo
    call mess(level_info, 'minimum Coriolis parameter : ', fcormin)
    call mess(level_info, 'maximum Coriolis parameter : ', fcormax)
    call doclose(mout)
 endif

 end subroutine inifcori

 subroutine orginifcori()
 use m_flowgeom
 use m_flow
 use m_sferic
 implicit none
 integer :: ierr, L, k, i, LL, LLL, LLLL, k1, k2, k3, n, j

 if (jsferic > 0) then
    if (allocated(fcori) ) then
       deallocate(fcori)
    endif

    if (icorio <= 6) then
       allocate ( fcori(lnx), stat = ierr )
       call aerr('fcori(lnx)', ierr, lnx  )
       do L = 1,lnx
          fcori(L) = 2d0*omega*sin(yu(L)*dg2rd)
       enddo
    else
       allocate ( fcori(ndx), stat = ierr )
       call aerr('fcori(ndx)', ierr, ndx  )
       do k = 1,ndx
          fcori(k) = 2d0*omega*sin(yz(k)*dg2rd)
       enddo
    endif

    if (jacorioconstant == 1) then
       fcori = 2d0*omega*sin(anglat*dg2rd)
    endif

    if ( jasecflow > 0 .and. kmx == 0 ) then

      ! Corilios in flow node, added by Nabi
       if (allocated(fcoris) ) then
          deallocate(fcoris)
       endif
       allocate ( fcoris(ndx), stat = ierr )
       call aerr('fcoris(ndx)', ierr, ndx  )
       do k = 1,ndx
          fcoris(k) = 2d0*omega*sin(yz(k)*dg2rd)
       enddo
    endif
 endif

 if (icorio > 40) then
    !if (allocated(LLkkk) ) then
    !   deallocate(LLkkk)
    !endif

    n = 0
    do j = 1,2  ! 1=count, 2=allocate and use
       if (j == 2) then
           allocate ( LLkkk(5,n) , stat = ierr ); LLkkk = 0
       endif
       n = 0
       do L = 1,lnx
          do i = 1,2
             k = ln(i,L)
             do LL   = 1, nd(k)%lnx                            ! loop over all attached links  k1,L1,k2,L2,k3
                LLL  = nd(k)%ln(LL)                            !                              ( 3  1  4  2  5, L)
                LLLL = iabs(LLL)
                if (L < LLLL) then
                   n = n + 1
                   if (j == 2) then
                      if (i == 2) then
                          k1 = ln(1,L)
                          k2 = ln(2,L)
                      else
                          k1 = ln(2,L)
                          k2 = ln(1,L)
                      endif
                      k3 = ln(1,LLLL) + ln(2,LLLL) - k2
                      LLkkk(1,n) = L
                      LLkkk(2,n) = LLLL
                      LLkkk(3,n) = k1
                      LLkkk(4,n) = k2
                      LLkkk(5,n) = k3
                   endif
                endif
             enddo
          enddo
       enddo
    enddo
 endif

 end subroutine orginifcori
