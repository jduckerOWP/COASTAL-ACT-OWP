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

! $Id: qucnu.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/qucnu.f90 $

 double precision function Qucnu(n12,L)              ! sum of (Q*uc cell centre upwind normal) at side n12 of link L
 use m_flow                                          ! advect the cell center velocities (dimension: m4/s2)
 use m_flowgeom                                      ! leaving the cell = +
 implicit none

 integer :: L                                        ! for link L,
 integer :: n12                                      ! find normal velocity components of the other links

 ! locals
 integer :: LL, LLL, LLLL                            ! for links LL,
 integer :: k12, kup                                 ! relevant node, 1 or 2, L/R
 double precision :: cs, sn, ucin, ucinx, uciny

 integer :: nn12

 double precision, external:: lin2nodx, lin2nody, nod2linx, nod2liny

 Qucnu = 0d0
 cs   = csu(L)
 sn   = snu(L)

 k12  = ln(n12,L)
 do LL   = 1, nd(k12)%lnx                            ! loop over all attached links
    LLL  = nd(k12)%ln(LL)
    LLLL = iabs(LLL)


    !if (qa(L) == 0d0 .or. L == LLLL ) then

    !else
       if ( qa(LLLL) > 0 ) then                      ! upwind cell centered velocity lies at kup=
          kup = ln(1,LLLL)
       else
          kup = ln(2,LLLL)
       endif
!       ucin = ucx(kup)*cs + ucy(kup)*sn - u1(L)
       nn12 = 1; if ( LLL.gt.0 ) nn12 = 2
       ucinx = lin2nodx(LLLL,nn12,ucxu(LLLL),ucyu(LLLL))
       uciny = lin2nody(LLLL,nn12,ucxu(LLLL),ucyu(LLLL))
       ucin  = nod2linx(L,n12,ucinx,uciny)*cs + nod2liny(L,n12,ucinx,uciny)*sn - u1(L)

       if (LLL > 0) then                             ! incoming link
          Qucnu = Qucnu - qa(LLLL)*ucin
       else
          Qucnu = Qucnu + qa(LLLL)*ucin
       endif
    !endif


 enddo

 end function Qucnu
