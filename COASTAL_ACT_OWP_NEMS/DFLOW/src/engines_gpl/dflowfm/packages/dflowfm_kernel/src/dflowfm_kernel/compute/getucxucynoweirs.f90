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

! $Id: getucxucynoweirs.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/getucxucynoweirs.f90 $

 subroutine getucxucynoweirs( ku, ucxku, ucyku, ischeme )
 use m_flow
 use m_flowgeom
 use m_sferic, only: jasfer3D
 implicit none

 integer           :: ku, LLL, LL, L, Ls, ischeme, n12

 double precision  :: ucxku, ucyku, ww, ac1, huweir, hunoweir, wl, wlno, at, cs, sn, fac

 double precision, external :: lin2nodx, lin2nody

 ucxku = 0d0  ; ucyku = 0d0
 huweir = 0d0 ; hunoweir = 0d0; wl = 0d0 ; wlno = 0d0; at = 0d0

 do LL = 1,nd(ku)%lnx
    Ls = nd(ku)%ln(LL); L = iabs(Ls)
    if (iadv(L) < 21 .or. iadv(L) > 29) then ! .ne. structures
       hunoweir = hunoweir + wu(L)*hu(L)
       wlno     = wlno     + wu(L)
    endif
 enddo
 if (wlno > 0d0 ) hunoweir = hunoweir/wlno

 do LL = 1,nd(ku)%lnx
    Ls = nd(ku)%ln(LL); L = iabs(Ls)
    if (Ls < 0) then
       ac1 = acL(L)
       n12 = 1
    else
       ac1 = 1d0 - acL(L)
       n12 = 2
    endif
    ww = ac1*dx(L)*wu(L)
    cs = ww*csu(L) ; sn = ww*snu(L)
    at = at + ww
    if (iadv(L) < 21 .or. iadv(L) > 29) then ! .ne. structures
       if (jasfer3D == 0) then
          ucxku = ucxku + cs*u0(L)
          ucyku = ucyku + sn*u0(L)
       else
          ucxku = ucxku + lin2nodx(L,n12,cs,sn)*u0(L)
          ucyku = ucyku + lin2nody(L,n12,cs,sn)*u0(L)
       endif
    else
       fac   = 1d0
       if (hunoweir > 0d0) fac = min(1d0, hu(L) / hunoweir )
       if (jasfer3D == 0) then
          ucxku = ucxku + cs*u0(L)*fac
          ucyku = ucyku + sn*u0(L)*fac
       else
          ucxku = ucxku + lin2nodx(L,n12,cs,sn)*u0(L)*fac
          ucyku = ucyku + lin2nody(L,n12,cs,sn)*u0(L)*fac
       endif
    endif
 enddo
 ucxku = ucxku/ba(ku)
 ucyku = ucyku/ba(ku)

 end subroutine getucxucynoweirs
