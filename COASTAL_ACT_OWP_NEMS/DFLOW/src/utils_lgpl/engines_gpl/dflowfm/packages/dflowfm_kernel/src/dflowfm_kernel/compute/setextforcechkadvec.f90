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

! $Id: setextforcechkadvec.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/setextforcechkadvec.f90 $

 subroutine setextforcechkadvec()
 use m_flow
 use m_flowparameters, only: trshcorio
 use m_flowgeom
 use m_netw
 use MessageHandling
 use m_alloc
 use m_wind
 use m_sferic
 use m_xbeach_data, only: Fx, Fy, swave, Lwave, hminlw, xb_started !, facmax, Trep

 implicit none

 integer          :: L,LL, Lb, Lt, k1,k2, kt1, kt2
 double precision :: dpatm, tidp, trshcorioi, fmax, floc, dzt, dztm, alf
 double precision :: GradHinUc
 double precision :: p1, p2, wfac, Dzk

 trshcorioi = 1d0/trshcorio

 
if (jawind > 0) then

    if (kmx == 0) then
        do LL = 1,lnx
           if ( hu(LL) > 0 ) then
               wfac = 1d0
               if (jawindpartialdry == 1) then
                  Dzk  = abs( zk(lncn(1,LL)) - zk(lncn(2,LL)) )
                  if (Dzk > 0d0) then
                     wfac = min( 1d0, hu(LL) / Dzk )
                  endif
               endif
               ! wdsu/huvli = [(m^2/s^2)*m^-1]
               if (jawindhuorzwsbased == 0) then
                  adve(LL) = adve(LL) - wdsu(LL)*wfac/hu(LL)
               else
                  adve(LL) = adve(LL) - wdsu(LL)*wfac*huvli(LL)
               endif
           endif
        enddo

    else

        do LL  = 1,lnx
           if (hu(LL) > 0d0) then
               wfac = 1d0
               if (jawindpartialdry == 1) then
                  Dzk  = abs( zk(lncn(1,LL)) - zk(lncn(2,LL)) )
                  if (Dzk > 0d0) then
                     wfac = min( 1d0, hu(LL) / Dzk )
                  endif
               endif

               Lt = Ltop(LL)
               ! adve(Lt) = adve(Lt) - wdsu(LL) / max( toplayminthick, hu(Lt) - hu(Lt-1)  )

               alf = 1d0
               if (jawindhuorzwsbased == 0) then
                  dzt = hu(Lt) - hu(Lt-1)
               else
                  kt1 = ktop( ln(1,LL) ) ; kt2 = ktop( ln(2,LL) )
                  dzt = acL(LL)*(zws(kt1) - zws(kt1-1)) + (1d0-acL(LL))*(zws(kt2) - zws(kt2-1))
               endif
               if ( Lbot(LL) < Lt ) then
                  dztm  =  hu(Lt-1) - hu(Lt-2)
                  !if ( dzt < 0.8d0*dztm ) then
                  if ( dzt < 0.05d0 ) then
                     alf   =  dzt / ( dzt + dztm )
                     adve(Lt-1) = adve(Lt-1) - (1d0-alf)*wdsu(LL)*wfac / dztm
                  endif
               endif
               adve(Lt) = adve(Lt) - alf*wdsu(LL)*wfac / dzt
           endif
        enddo

    endif

 endif

 if ((jawave==3.or.jawave==6) .and. .not. flowWithoutWaves) then
     ! if a SWAN computation is performed, add wave forces to adve
     ! This part is mainly based on the wave forces formulation (wsu) of Delft3D (cucnp.f90)

    if ( kmx.eq.0 ) then  ! 2D
       do L  = 1,lnx
          adve(L) = adve(L) - wavfu(L)
       enddo
    else
       do LL  = 1,lnx
          call getLbotLtop(LL,Lb,Lt)
          do L=Lb,Lt
             adve(L) = adve(L) - wavfu(L)           ! Dimensions [m/s^2]
          end do
       enddo
    end if
 endif

! JRE
 if (jawave .eq. 4) then                              ! wave forcing from XBeach
    call xbeach_wave_compute_flowforcing2D()
    if (lwave==1)  then
       do L  = 1,Lnx
          floc = Fx(L)*csu(L) + Fy(L)*snu(L)
          adve(L) = adve(L) - floc/ (rhomean*max(hu(L), hminlw) )    ! Johan+Dano: lower depth set to 20cm, cf XBeach
       enddo
    endif
 endif
   
 if (japatm > 0 .or. jatidep > 0) then
    do L  = 1,lnx
       if ( hu(L) > 0 ) then
          k1     = ln(1,L) ; k2 = ln(2,L)

          if (japatm > 0) then
!             dpatm  = ( patm(k2) - patm(k1) )*dxi(L)/rhomean
!             if ( hu(L) < trshcorio ) then
!                 dpatm  = dpatm*hu(L)*trshcorioi
!             endif

             dpatm  = (patm(k2)-patm(k1))*dxi(L)/rhomean

             if (kmx == 0) then
                adve(L) = adve(L) + dpatm
             else
                do LL = Lbot(L), Ltop(L)
                   adve(LL) = adve(LL) + dpatm
                enddo
             endif

          endif
     
          if (jatidep > 0 .or. jaselfal > 0) then
             tidp  = ( tidep(1,k2) - tidep(1,k1) )*dxi(L)
             if ( hu(L) < trshcorio) then
                tidp = tidp*hu(L)*trshcorioi
             endif
             if (kmx == 0) then
                adve(L) = adve(L) - tidp
             else
                do LL = Lbot(L), Ltop(L)
                   adve(LL) = adve(LL) - tidp
                enddo
             endif

 !           add to tidal forces
             tidef(L) = tidp
           endif
       endif
    enddo
    
    if ( jatidep.gt.0 .or. jaselfal.gt.0 .and. kmx.eq.0 ) then
       call comp_GravInput()
    end if

 endif

 ! Anti-creep
 if( kmx < 2 .and. jacreep == 1) then           ! A warning due to kmx<2 and anticreep on
     call mess(LEVEL_INFO, 'Error : Anti-creep must be switched off in a 1d/2d model!')
 endif

 if ( idensform > 0) then                                     ! Baroclinic pressure
    if ( jacreep == 1) then
       dsalL = 0d0
       dtemL = 0d0
       do L = 1,lnx
          if (hu(L) > 0d0) then
             call anticreep( L )
          endif
       enddo

     else

       call addbaroclinicpressure()

       if (abs(jabaroctimeint) == 2) then
           rho0 = rho                                           ! save rho
       else if (abs(jabaroctimeint) == 5) then
           if (jarhoxu > 0) then
              rho = rho0                                        ! restore rho
           endif
       endif
       jabaroctimeint = abs(jabaroctimeint)                     ! flag as initialised

    end if
 endif

 if ( jasecflow > 0 ) then                                         ! Secondary Flow

    do LL = 1,lnx
       call getcz( hu(LL), frcu(LL), ifrcutp(LL), czusf(LL), LL)   ! calculating chezy coefficient on the flow links
    enddo

    !
    if (kmx < 2) then
       call linkstocenterstwodoubles( czssf, czusf )               ! converting chezy cofficient to the flow nodes
       if( spirbeta > 0.0d0 ) then
          call get_spiralforce()
       endif
    else
       !call linkstocenterstwodoubles( czssf, czusf )
       call get_spiral3d()                                           ! compute equivalent secondary flow intensity
    endif
 end if
 
 if ( jaFrcInternalTides2D.gt.0 .and. kmx.eq.0 ) then   ! internal tides friction (2D only)
    call add_InternalTidesFrictionForces()
 end if
 
 if (chkadvd > 0) then                       ! niet droogtrekken door advectie, stress of wind (allen in adve)

    if (kmx == 0) then

       do L  = 1,lnx

          if ( hu(L) > 0 ) then
             k1      = ln(1,L) ; k2 = ln(2,L)

             if     (hs(k1)  < 0.5d0*hs(k2) ) then
                if (adve(L)  < 0 .and. hs(k1) < chkadvd ) then
                    adve(L)  = adve(L)*hs(k1) / chkadvd ! ; nochkadv = nochkadv + 1
                endif
             else if (hs(k2) < 0.5d0*hs(k1) ) then
                if (adve(L)  > 0 .and. hs(k2) < chkadvd ) then
                    adve(L)  = adve(L)*hs(k2) / chkadvd ! ; nochkadv = nochkadv + 1
                endif
             endif
          endif

       enddo

    else

       do LL  = 1,lnx
           if (hu(LL) > 0d0) then
              call getLbotLtop(LL,Lb,Lt)
              k1   = ln(1,LL) ; k2 = ln(2,LL)
              if      (hs(k1) < 0.5d0*hs(k2) ) then
                 do L = Lb, Lt
                    if (adve(L)  < 0 .and. hs(k1) < chkadvd ) then
                       adve(L)  = adve(L)*hs(k1) / chkadvd ! ; nochkadv = nochkadv + 1
                    endif
                 enddo
              else if (hs(k2) < 0.5d0*hs(k1) ) then
                 do L = Lb, Lt
                    if (adve(L)  > 0 .and. hs(k2) < chkadvd) then
                        adve(L)  = adve(L)*hs(k2) / chkadvd ! ; nochkadv = nochkadv + 1
                    endif
                 enddo
              endif
          endif
       enddo

    endif

 endif
 
 end subroutine setextforcechkadvec
