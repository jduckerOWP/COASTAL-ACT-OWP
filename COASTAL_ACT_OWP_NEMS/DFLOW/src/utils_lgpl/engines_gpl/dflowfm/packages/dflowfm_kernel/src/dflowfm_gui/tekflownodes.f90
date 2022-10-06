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

! $Id: tekflownodes.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/tekflownodes.f90 $

 subroutine tekflownodes(ja)
 use unstruc_display
 use m_flowgeom
 use m_flow
 use m_missing
 use m_transport
 implicit none
 integer :: nodemode, nodewhat,ndraw(50)
 integer :: k, ja, ja2, nn, ncol
 double precision :: znod, zn, x(8), y(8)
 common /drawthis/ ndraw
 logical inview


 nodemode = ndraw(19)
 nodewhat = ndraw(28)
 ja = 0

 if (nodemode == 3) then               ! interpolate rnod on netnodes based upon znod on flownodes
    call copyznodtornod()
 endif
 do k = 1,ndxi
    if (mod(k,200) == 0) then
       call halt(ja)
       if (ja == 1) then
          return
       endif
    endif
    if (nodewhat .ge. 2) then
       ja2 = 1
       if (wetplot > 0d0) then
          if (hs(k) < wetplot) then
             ja2 = 0
          endif
       endif
       if (ja2 == 1 .or. nodewhat == 3) then  ! nodewhat==3: always show bottom
          if (inview( xz(k), yz(k) ) ) then
             zn = znod(k)
             if ( zn.eq.DMISS ) cycle
             if (nodemode .eq. 2) then
                call isocol(zn,ncol)
                call dhtext( zn, xz(k), yz(k), bl(k) )
             else if (nodemode == 3   .or. nodemode == 3 + 3) then    ! isolines within cell
                if (k <= ndx2d) then
                   call ISOSMOOTHflownode(k)
                else
                   call isocol(zn,ncol)
                   nn = size( nd(k)%x )
                   call PFILLER(nd(k)%x, nd(k)%y, nn,NCOL,NCol)
                endif
             else if (nodemode .ge. 4 .or. nodemode == 4 + 3) then  ! isofil= cellfill
                call isocol(zn,ncol)
                if ( nodemode == 5    .or. nodemode == 5 + 3 ) then
                   call drcirc(xz(k), yz(k), zn)
                else
                   nn = size( nd(k)%x )
                   call PFILLER(nd(k)%x, nd(k)%y, nn,NCOL,NCol)
                endif
             endif
          endif
       endif
    endif
 enddo
 end subroutine tekflownodes
