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

! $Id: get_dtmax.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_transport/get_dtmax.f90 $

!> get maximum timestep for water columns (see setdtorg)
subroutine get_dtmax()
   use m_flowgeom, only: Ndx, Ndxi, bl, nd, Dx, ln, lnx
   use m_flow, only: s1, epshu, squ, sqi, vol1, kmx, u1, hu, diusp, viu, Lbot, Ltop
   use m_flowparameters, only: eps10, cflmx, jadiusp
   use m_turbulence, only: sigdifi
   use m_flowtimes, only: time1
   use m_physcoef, only:  dicouv
   use m_timer
   use m_transport
   use m_partitioninfo
   use timers

   implicit none

   double precision                              :: difcoeff
   double precision                              :: diuspL

   integer                                       :: kk, k, kb, kt
   integer                                       :: L, LL, Lb, Lt
   integer                                       :: k1, k2
   integer                                       :: j
   integer                                       :: ierror

   double precision,                 parameter   :: dtmax_default = 1d4

   integer(4) ithndl /0/
   if (timon) call timstrt ( "get_dtmax", ithndl )

   dtmin_transp = huge(1d0)
   kk_dtmin = 0

   if ( jalimitdtdiff.eq.1 ) then
!     determine contribution of diffusion to time-step limitation, mosly copied from "comp_fluxhor3D"
      sumdifflim = 0d0
      do LL=1,Lnx
         if (jadiusp == 1) then
             diuspL = diusp(LL)
         else
             diuspL = dicouv
         endif

         Lb = Lbot(LL)
         Lt = Ltop(LL)

         do L=Lb,Lt
            k1 = ln(1,L)
            k2 = ln(2,L)

            difcoeff = 0d0


!           compute maximum diffusion coefficient
            do j=1,NUMCONST
!              compute diffusion coefficient (copied from "comp_fluxhor3D")
               difcoeff  = max(difcoeff, sigdifi(j)*viu(L) + difsedu(j) + diuspL)  ! without smagorinsky, viu is 0 ,
                                                                                   ! difsed only contains molecular value,
                                                                                   ! so then you only get user specified value
            end do

            sumdifflim(k2) = sumdifflim(k2) + difcoeff*dxiAu(L)
            sumdifflim(k1) = sumdifflim(k1) + difcoeff*dxiAu(L)
         end do
      end do
   end if

   if ( kmx.eq.0 ) then

      do k=1,Ndxi
         dtmax(k) = dtmax_default

!         if ( s1(k)-bl(k).gt.epshu ) then

            if ( jalimitdtdiff.eq.0 ) then
               if ( squ(k).gt.eps10 ) then
                  dtmax(k) = min(dtmax(k),cflmx*vol1(k)/squ(k))
               end if
            else
               if ( sqi(k)+sumdifflim(k).gt. eps10 ) then
                  dtmax(k) = min(dtmax(k), cflmx*vol1(k)/(sqi(k)+sumdifflim(k)))
!                  dtmax = min(dtmax(k), cflmx*vol1(k)/(squ(k)+sumdifflim(k)))
               end if
            end if

! BEGIN DEBUG
!            do LL=1,nd(k)%lnx
!               L = iabs(nd(k)%ln(LL))
!               if ( hu(L).gt.0d0 .and. u1(L).gt.0d0 ) then
!                  dtmax(k) = min(dtmax(k),cflmx*Dx(L)/u1(L))
!               end if
!            end do
! END DEBUG

            if ( jampi.eq.1 ) then
!              do not include ghost cells
               if ( idomain(k).ne.my_rank ) cycle
            end if

            if ( dtmax(k).lt.dtmin_transp ) then
               dtmin_transp = dtmax(k)
               kk_dtmin = k
            end if
!         end if

      end do

   else

      do kk=1,Ndxi
         dtmax(kk) = dtmax_default

         if ( s1(kk)-bl(kk).gt.epshu ) then
            call getkbotktop(kk,kb,kt)
            if ( jalimitdtdiff.eq.0 ) then
               do k=kb,kt
                  if ( squ(k).gt.eps10 .or. sqi(k).gt.eps10 ) then
                     dtmax(kk) = min(dtmax(kk),vol1(k)/max(squ(k),sqi(k)))
                  end if
               end do
            else
               do k=kb,kt
                  if ( sqi(k)+sumdifflim(k).gt.eps10 ) then
                     dtmax(kk) = min(dtmax(kk),vol1(k)/(sqi(k)+sumdifflim(k)))
!                     dtmax(kk) = min(dtmax(kk),vol1(k)/(squ(k)+sumdifflim(k)))
                  end if
               end do
            end if
            dtmax(kk) = cflmx*dtmax(kk)

            if ( jampi.eq.1 ) then
!              do not include ghost cells
               if ( idomain(kk).ne.my_rank ) cycle
            end if

            if ( dtmax(kk).lt.dtmin_transp ) then
               dtmin_transp = dtmax(kk)
               kk_dtmin = kk
            end if
         end if

      end do

   end if

   time_dtmax = time1

   if ( jampi.eq.1 ) then
!     update dtmax
      call update_ghosts(ITYPE_Sall, 1, Ndx, dtmax, ierror)
!     globally reduce maximum time-step
      if ( jatimer.eq.1 ) call starttimer(IMPIREDUCE)
      call reduce_double_min(dtmin_transp)
      if ( jatimer.eq.1 ) call stoptimer(IMPIREDUCE)
   end if

   if (timon) call timstop( ithndl )
   return
end subroutine get_dtmax
