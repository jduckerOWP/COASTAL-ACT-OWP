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

! $Id: addlink1dkcu3.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/addlink1dkcu3.f90 $

 subroutine addlink1Dkcu3(L,japerim)                        ! and add area's and volumes of 1D link kcu3
 use m_flowgeom
 use m_flow
 use m_missing
 use unstruc_channel_flow
 use precision_basics

 implicit none

 integer           :: japerim, L, ja

 integer           :: k1, k2, K, calcConv
 double precision  :: ar1, wid1, cf1, ar2, wid2, cf2, dx1, dx2, widu, perim
 double precision  :: hpr
 double precision, external :: get_hpr_nostruc

 if (japerim == 0) then
    calcConv = 0
    k1  = ln(1,L) ; k2 = ln(2,L)

    hpr = max(0d0,s1(k1)-bl(k1))    ! this statement is called most nr of times through waterlevel iteration
    if (hpr > 0) then                             !
       call getprof_1D(L, hpr, ar1, wid1, japerim, calcConv, perim)
       dx1  = dx(L)*acl(L)
       a1(k1) =   a1(k1) + dx1*wid1
       vol1(k1)   = vol1(k1)   + dx1*ar1
    endif

    hpr = max(0d0,s1(k2)-bl(k2))
    if (hpr > 0) then                             !
       call getprof_1D(L, hpr, ar2, wid2, japerim, calcConv, perim)
       dx2  = dx(L)*(1d0-acl(L))
       a1(k2) =   a1(k2) + dx2*wid2
       vol1(k2)   = vol1(k2)   + dx2*ar2
    endif

 else
    if (hu(L) > 0) then
       !DIR$ INLINE
       hpr = get_hpr_nostruc(L)
       calcConv = 1
       call getprof_1D(L, hpr, au(L), widu, japerim, calcConv, perim)
    endif

    calcConv = 0
    if(network%loaded) then
       ! Only in case of a 1d-network, vol1 and vol1_f can be different
       if (kcs(k1) == 1) then ! TODO: consider *also* adding storage area to the 2D side k1, if kcu(L)==5, maybe not for kcu(L)==7
          hpr = s1(k1)-bob0(1,L)
          if (hpr >= 0d0) then
             if (comparereal(hu(L), hpr)== 0) then
                vol1_f(k1) = vol1_f(k1) + dx1*au(L)
             else
                call getprof_1D(L, hpr, ar1, wid1, 1, calcConv, perim)
                vol1_f(k1) = vol1_f(k1) + dx1*ar1
             endif
          endif
       endif
       if (kcs(k2) == 1) then ! TODO: consider *also* adding storage area to the 2D side k2, if kcu(L)==5, maybe not for kcu(L)==7
          hpr = s1(k2)-bob0(2,L)
          if (hpr >= 0d0) then
             ! flow volume
             if (comparereal(hu(L), hpr)== 0) then
                vol1_f(k2) = vol1_f(k2) + dx2*au(L)
             else
                call getprof_1D(L, hpr, ar2, wid2, 1, calcConv, perim)
                vol1_f(k2) = vol1_f(k2) + dx2*ar2
             endif
          endif
       endif
    else

       vol1_f(k1) = vol1(k1)
       vol1_f(k2) = vol1(k2)
    endif

 endif
 end subroutine addlink1Dkcu3
