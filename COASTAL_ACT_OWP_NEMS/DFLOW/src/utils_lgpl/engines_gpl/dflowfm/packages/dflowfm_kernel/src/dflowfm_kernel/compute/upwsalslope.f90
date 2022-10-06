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

! $Id: upwsalslope.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/upwsalslope.f90 $

double precision function upwsalslope(L,k,ds2)          ! k is upwind cell for link L, find slope upwind of k
 use m_flowgeom                                          ! limit upwind slopes for all inflowing links
 use m_flow
 implicit none
 integer          :: L, k
 double precision :: ds2


 integer                    :: kk,ku,LL,LLL,jap
 double precision           :: ds1
 double precision, external :: dcminmod

 upwsalslope = -1d9
 if (ds2 < 0) upwsalslope = 1d9

 jap = -1
 if (ln(1,L) == k) jap = 1

 do kk = 1,nd(k)%lnx
    LLL= nd(k)%ln(kk)
    LL = iabs(LLL)
    if (LL .ne. L .and. q1(LL)*LLL > 0) then
       ku = ln(1,LL)
       if (ku == k) ku = ln(2,LL)

       ds1 = (sa0(k) - sa0(ku))*jap

       if (ds2 > 0) then
          upwsalslope = dcminmod(ds1,ds2)
       else if (ds2 < 0) then
          upwsalslope = dcminmod(ds1,ds2)
       endif
    endif
 enddo
 end function upwsalslope
