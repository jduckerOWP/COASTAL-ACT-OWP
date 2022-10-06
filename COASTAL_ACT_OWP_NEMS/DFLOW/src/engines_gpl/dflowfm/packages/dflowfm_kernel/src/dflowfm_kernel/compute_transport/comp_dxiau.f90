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

! $Id: comp_dxiau.f90 140737 2022-02-10 16:03:14Z spee $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_transport/comp_dxiau.f90 $

! compute Au/Dx for diffusive flux
subroutine comp_dxiAu()                          ! or: setdxiau
   use m_flowgeom , only : ln, Lnx, dxi, wu, Lnxi
   use m_flow     , only : hs, zws, kmx, Au, hu, jadiffusiononbnd
   use m_transport, only : dxiAu, jalimitdtdiff
   use timers

   implicit none

   integer :: k1, k2
   integer :: LL, L, Lb, Lt
   integer(4) ithndl /0/

   if (timon) call timstrt ( "comp_dxiAu", ithndl )

   if ( jalimitdtdiff.eq.0 ) then
      if ( kmx.eq.0 ) then
         do L=1,Lnx
            dxiAu(L) = dxi(L)*Au(L)
         end do
      else
         do LL=1,Lnx
            call getLbotLtop(LL,Lb,Lt)
            do L=Lb,Lt
               dxiAu(L) = dxi(LL)*Au(L)
            end do
         end do
      end if
   else
      if ( kmx.eq.0 ) then
         do L=1,Lnx
            if (au(L) > 0d0) then 
               k1 = ln(1,L)
               k2 = ln(2,L)
               !dxiAu(L) = dxi(L)*wu(L) * min(hs(k1), hs(k2))
               dxiAu(L) = dxi(L)*wu(L)*min( hs(k1), hs(k2),hu(L) )
            else 
               dxiAu(L) = 0d0
            endif
         end do
      else
         do LL=1,Lnx
            call getLbotLtop(LL,Lb,Lt)
            do L=Lb,Lt
               if (au(L) > 0d0) then 
                  k1 = ln(1,L)
                  k2 = ln(2,L)
                  !dxiAu(L) = dxi(LL)*wu(LL) * min(zws(k1)-zws(k1-1),zws(k2)-zws(k2-1))
                  dxiAu(L) = dxi(LL)*wu(LL) * min(zws(k1)-zws(k1-1),zws(k2)-zws(k2-1), hu(L)-hu(L-1))
               else
                  dxiAu(L) = 0d0
               endif
            end do
         end do
      end if
   end if

   if (jadiffusiononbnd == 0) then  
      do LL=lnxi+1, lnx
         call getLbotLtop(LL,Lb,Lt)
         do L=Lb,Lt
            dxiAu(L) = 0d0 
         enddo
      enddo
   endif

   if (timon) call timstop( ithndl )
   return
end subroutine comp_dxiAu
