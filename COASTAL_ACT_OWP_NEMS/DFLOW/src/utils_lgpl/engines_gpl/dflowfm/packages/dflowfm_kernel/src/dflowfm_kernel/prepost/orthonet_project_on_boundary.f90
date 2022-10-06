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

! $Id: orthonet_project_on_boundary.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/orthonet_project_on_boundary.f90 $

!> project boundary-nodes back to the boundary of an original net
subroutine orthonet_project_on_boundary(nmkx, kk1, k_bc, xkb, ykb)
   use m_netw

   IMPLICIT NONE

   integer                           :: nmkx     !< maximum number of link-connected neighboring nodes
   integer, dimension(numk)          :: k_bc     !< maps nodes to nearest original boundary nodes
   double precision, dimension(numk) :: xkb, ykb !< copy of the original net
   integer, dimension(nmkx,numk)     :: kk1      !< link-connected neighboring nodes

   double precision                  :: x0, y0, xl, yl, xr, yr
   double precision                  :: x2, y2, x3, y3, xn2, yn2, xn3, yn3
   double precision                  :: dis2, dis3, r2, r3

   integer                           :: k, kk, k0, kL, kR, nr, ja2, ja3

   do k0 = 1,numk
      if ( nb(k0).eq.2 .and. nmk(k0).gt.0) then
         k  = k_bc(k0)        ! the nearest node in the original net, in previous iteration
         if ( nmk(k).eq.0 ) cycle
         x0 = xk(k0)
         y0 = yk(k0)
         nr = 0
         kr = -999
         do kk = 1,nmk(k)
            if (lnn(nod(k)%lin(kk)) == 1) then
                ! remember the two boundary neighbours in original net.
                nr = nr + 1
                if (nr == 1) then
                    kL = kk1(kk,k)
                    if ( kL.eq.0 ) then
                       return !  should not happen
                    end if
                    x2 = xkb(kl) ; y2 = ykb(kl)
                else if (nr == 2) then
                    kR = kk1(kk,k)
                    if ( kR.eq.0 ) then
                       return !  should not happen
                    end if
                    x3 = xkb(kr) ; y3 = ykb(kr)
                endif
            end if
         enddo

! Project the moved boundary point back onto the closest
! ORIGINAL edge (netlink) (either between 0 and 2 or 0 and 3)
         call dlinedis3(x0,y0,xkb(k),ykb(k),x2,y2,ja2,dis2,xn2,yn2,r2)
         call dlinedis3(x0,y0,xkb(k),ykb(k),x3,y3,ja3,dis3,xn3,yn3,r3)
         if (dis2 < dis3) then
            x0 = xn2 ; y0 = yn2
            if ( (r2.gt.0.5d0) .and. (nb(kL).ne.3) ) k_bc(k0) = kL
         else
            x0 = xn3 ; y0 = yn3
            if ( (r3.gt.0.5d0) .and. (nb(kR).ne.3) ) k_bc(k0) = kR
         endif

         xk(k0) = x0 ; yk(k0) = y0

      endif
   enddo

end subroutine orthonet_project_on_boundary
