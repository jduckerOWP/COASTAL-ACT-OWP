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

! $Id: getequilibriumtransportrates2.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_sediment/netnode_based/getequilibriumtransportrates2.f90 $

 subroutine getequilibriumtransportrates2(L, kb1, kb2, seq, wse, mx, hsk, jamin) ! get equilibrium transportrateconc seq based on bans associated with bndlink L
 use m_netw
 use m_flowgeom
 use m_sediment
 implicit none
 integer           :: L, kb1, kb2, mx, jamin                              ! Linknr, left and right ban nr, mxgr,
 double precision  :: seq(mx)  , seq2(mx)                                 ! seq(kg/m3)
 double precision  :: wse(mx)                                             ! effective fall velocity (m/s)
 double precision  :: hsk                                                 ! waterdepth, flowcell or ban
 integer           :: k1, k2, kk, n, j

 if (kb1 == 0) then        ! if bans unknown, first find them
    k1 = lncn(1,L) ; k2 = lncn(2,L)
    do kk = 1,mxban
       n = nban(1,kk)      ! net node
       if (kb1 == 0) then
          if (n == k1) then
             kb1 = kk
          endif
       endif
       if (kb2 == 0) then
          if (n == k2) then
             kb2 = kk
          endif
       endif
       if (kb1 .ne. 0 .and. kb2 .ne. 0) then
           exit
       endif
    enddo
 endif

 if (kb1 == 0) then
     kb1 = 0
 endif
 if (kb2 == 0) then
     kb2 = 0
 endif


 call getequilibriumtransportrates(kb1, seq , wse, mx, hsk)

 call getequilibriumtransportrates(kb2, seq2, wse, mx, hsk)

 if (jamin == 1) then
    do j = 1,mxgr
       seq(j) = min( seq(j), seq2(j) )
    enddo
 else
    do j = 1,mxgr
       seq(j) = max( seq(j), seq2(j) )
    enddo
 endif

 end subroutine getequilibriumtransportrates2
