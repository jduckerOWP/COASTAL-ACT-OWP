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

! $Id: setequilibriumsedimentbnds.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_sediment/netnode_based/setequilibriumsedimentbnds.f90 $

Subroutine setequilibriumsedimentbnds(nbnd,n4,kbnd,kban,i01)
use m_flow
use m_flowgeom
use m_sediment
implicit none

integer           :: nbnd, kban(2,nbnd) , kbnd(n4,nbnd), i01, n4

integer           :: k, kb, ki, L, LL, Lb, Lt, j
double precision  :: hsk
double precision  :: seq  (mxgr)                !< sed equilibrium transport rate (kg/m/s) , dimension = mxgr
double precision  :: wse  (mxgr)                !< effective fall velocity (m/s)           , dimension = mxgr, ws*crefa=wse*seq



do k  = 1,nbnd                                  ! set equilibrium boundary conditions for open flow bnds, types z and u
   kb = kbnd(1,k)
   ki = kbnd(2,k)
   LL = kbnd(3,k)

   if (q1(LL) < 0) then
      bl(kb) = bl(ki)                            ! copy internal bottom level to outflow bnd level
      if (jaceneqtr == 1) then
         do j = 1,mxgr
            grainlay(j,kb) = grainlay(j,ki)
         enddo
      endif
   endif

   if (jaceneqtr == 1) then
      call getequilibriumtransportrates(ki, seq, wse, mxgr, hsk)                            ! get based on cellcentre
   else
      call getequilibriumtransportrates2(LL, kban(1,k), kban(2,k), seq, wse, mxgr, hsk, i01) ! get based on 2 netnodes
   endif
   call getLbotLtop(LL,Lb,Lt)
   do L  = Lb,Lt
      kb = ln(1,L) ; ki = ln(2,L)
      do j = 1,mxgr
         if (q1(L) > 0) then
            sed(j,kb) = seq(j)                      ! inflow ,  equilibrium boundary condition
         else
            sed(j,kb) = sed(j,ki)                   ! outflow
         endif
      enddo
   enddo
enddo

End Subroutine setequilibriumsedimentbnds
