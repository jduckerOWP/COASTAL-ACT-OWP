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

! $Id: apply_tracer_bc.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_transport/apply_tracer_bc.f90 $

!> apply tracer boundary conditions
subroutine apply_tracer_bc()
   use m_transport
   use m_meteo
   use m_flowgeom, only: ln
   use m_flow, only: kmxd, q1, kmxL
   use timers
   implicit none

   character (len=NAMTRACLEN)    :: tracnam

   double precision :: valtop

   integer :: itrac, iconst
   integer :: k, kk, ki, kb
   integer :: L, LL, Lb, Lt

   integer(4) ithndl /0/
   if (timon) call timstrt ( "apply_tracer_bc", ithndl )

!  loop over the tracer boundary conditions
   do itrac=1,numtracers
      iconst = itrac2const(itrac)
      do k=1,nbndtr(itrac)
         LL = bndtr(itrac)%k(3,k)
         call getLbotLtop(LL,Lb,Lt)
         kb = 0
         do L = Lb,Lt
            kb = ln(1,L)
            ki = ln(2,L)
            if ( q1(L).gt.0 ) then  ! inflow
               kk = kmxd*(k-1)+L-Lb+1
               constituents(iconst,kb) = bndtr(itrac)%z(kk)
            else                    ! outflow
               constituents(iconst,kb) = constituents(iconst,ki)
            end if
         end do

         if ( kb.gt.0 ) then
            valtop = constituents(iconst,kb)

            do L=Lt+1,Lb+kmxL(LL)-1
               kb = ln(1,L)
               ki = ln(2,L)
               if ( q1(Lt).gt.0d0 ) then
                  constituents(iconst,kb) = valtop
               else
                  constituents(iconst,kb) = constituents(iconst,ki)
               end if
            end do
         end if
      end do
   end do

   if (timon) call timstop( ithndl )
   return
end subroutine apply_tracer_bc
