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

! $Id: tekrailines.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/tekrailines.f90 $

 subroutine tekrailines(ncol,jaall,ITYP)
 use m_flowgeom
 USE M_FLOW
 use m_flowtimes
 use m_sferic
 use m_missing
 use unstruc_display
 use m_transport
 implicit none
 integer          :: nx, ncol, jaall, ITYP
 integer          :: r, L, k1,k2
 double precision :: zz1, zz2, xz1, xz2
 integer          :: ja

 call setcol(ncol)
 do L = 1,lnx
    if (mod(L,200) == 0) then
       call halt2(ja)
       if (ja == 1) exit
    endif


    k1 = ln (1,L)
    k2 = ln (2,L)

    if (jaall == 1 .and. wetplot > 0d0) then
       if (hu(L) < wetplot) then !  hs(k1) < wetplot .or. hs(k2) < wetplot) then
           cycle
       endif
    endif

    zz1 = dmiss ; zz2 = dmiss
    if (ityp == 1) then
       zz1 = s1(k1)
       zz2 = s1(k2)
    else if (ityp == 2) then
       zz1 = bl(k1)
       zz2 = bl(k2)
    else if (ityp == 3) then
       zz1 = constituents(isalt,k1)
       zz2 = constituents(isalt,k2)
    else if (ityp == 4) then
       zz1 = pgrw(k1)
       zz2 = pgrw(k2)
    else if (ityp == 5) then
       zz1 = sgrw1(k1)
       zz2 = sgrw1(k2)
    else if (ityp == 6) then
       if (L <= lnx1D) then
          zz1 = dmiss ; zz2 = dmiss
          if (prof1D(3,L) < 0) then
             if ( s1m(k1) > bl(k1) + prof1D(2,L) .or. s1m(k2) > bl(k2) + prof1D(2,L)) then
                zz1 = s1m(k1)
                zz2 = s1m(k2)
             endif
          endif
       endif
    else if (ityp == 7) then
       if (L <= lnx1D) then
          if (prof1D(1,L) > 0) then
             zz1 = bl(k1) + prof1D(2,L)
             zz2 = bl(k2) + prof1D(2,L)
          else
             zz1 = bl(k1)
             zz2 = bl(k2)
          endif
       else
          zz1 = bl(k1)
          zz2 = bl(k2)
       endif
    endif

    if (zz1 == dmiss .or. zz2 == dmiss) cycle

    if (yfac > 0) then
       zz1 = zz1 + (yz(k1) - ymn)*yfac
       zz2 = zz2 + (yz(k2) - ymn)*yfac
    endif

    if (jsferic == 1) then ! jglobe
       if (abs(xz(k1) - xz(k2)) > 10d0) cycle
    endif

    xz1 = xz(k1)
    xz2 = xz(k2)

    if (abs(zz1) < 1d-6) zz1 = 0d0  ! heh heh, eindelijk. -> #@!
    if (abs(zz2) < 1d-6) zz2 = 0d0

    call movabs(xz1, zz1 )
    call  lnabs(xz2, zz2 )

 enddo

 end subroutine tekrailines
