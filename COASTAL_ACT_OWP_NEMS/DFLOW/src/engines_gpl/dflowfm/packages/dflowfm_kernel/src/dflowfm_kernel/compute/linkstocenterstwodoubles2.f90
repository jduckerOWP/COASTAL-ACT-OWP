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

! $Id: linkstocenterstwodoubles2.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/linkstocenterstwodoubles2.f90 $

 subroutine linkstocenterstwodoubles2(vnod,vlin,vlin2)      ! both vlin and vlin2 to vnod(1,* and vnod(2,*
 use m_flow
 use m_netw
 use m_flowgeom

 implicit none

 double precision       :: vlin(lnkx), vlin2(lnkx)
 double precision       :: vnod(2,ndkx)
 integer                :: L, k1, k2, LL, Lb, Lt, kk, kb, kt, k

 vnod = 0d0
 if (kmx == 0) then
    do L   = 1,lnx
       k1  = ln  (1,L) ; k2 = ln  (2,L)
       vnod(1,k1) = vnod(1,k1) + vlin (L)*wcL(1,L)
       vnod(1,k2) = vnod(1,k2) + vlin (L)*wcL(2,L)
       vnod(2,k1) = vnod(2,k1) + vlin2(L)*wcL(1,L)
       vnod(2,k2) = vnod(2,k2) + vlin2(L)*wcL(2,L)
    enddo
 else
    do LL  = 1,lnx
       call getLbotLtop(LL,Lb,Lt)
       do L = Lb,Lt
          k1  = ln  (1,L) ; k2 = ln  (2,L)
          vnod(1,k1) = vnod(1,k1) + vlin (L)*wcL(1,LL)
          vnod(1,k2) = vnod(1,k2) + vlin (L)*wcL(2,LL)
          vnod(2,k1) = vnod(2,k1) + vlin2(L)*wcL(1,LL)
          vnod(2,k2) = vnod(2,k2) + vlin2(L)*wcL(2,LL)
       enddo
    enddo

    !$OMP PARALLEL DO                                          &
    !$OMP PRIVATE(kk,kb,kt,k)
    do kk = 1,ndx
       call getkbotktop(kk,kb,kt)
       do k = kt+1, kb+kmxn(kk)-1
          vnod(1,k) = vnod(1,kt)
          vnod(2,k) = vnod(2,kt)
       enddo
    enddo
    !$OMP END PARALLEL DO
 endif
 end subroutine linkstocenterstwodoubles2
