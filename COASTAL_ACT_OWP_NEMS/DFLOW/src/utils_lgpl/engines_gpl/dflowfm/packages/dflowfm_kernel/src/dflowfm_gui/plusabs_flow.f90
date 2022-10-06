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

! $Id: plusabs_flow.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/plusabs_flow.f90 $

!
 Subroutine plusabs_flow(numchoice)
 use m_flow
 use m_flowgeom
 use m_transport, only: NUMCONST, ISALT, ITEMP, ISED1, ISEDN, ITRA1, ITRAN, ITRAN0, constituents, itrac2const, const_names, const_units

 implicit none

 integer :: numchoice, k, kk, kb, kt

 ! locals
 integer :: key

 if (ndx == 0 .or. lnx == 0) then
    call qnerror('First reinitialise flow model, current dimensions are 0',' ',' ')
    return
 endif

 if (numchoice == 1) then
    call plusabsd(xz,yz,yz,ndx,key,s1); s1=max(s1,bl)
 else if (numchoice == 2 .and. jasal >0)  then
    if (.not. allocated (sa1) ) then
       allocate(sa1(ndx))
    endif
    do kk = 1,ndx
       sa1(kk) = constituents(isalt,kk)
    enddo
    call plusabsd(xz,yz,yz,ndx,key,sa1)
    do kk = 1,ndx
       call getkbotktop(kk,kb,kt)
       do k = kb,kt
          constituents(isalt,k) = sa1(kk)
       enddo
       constituents(isalt,kk) = sa1(kk)
    enddo

    salmax = maxval(sa1)

 else if (numchoice == 3) then
    if (ibedlevtyp == 1) then
       call plusabsd(xz,yz,yz,ndx,key,bl)
    else if (ibedlevtyp == 2) then
       call plusabsd(xu,yu,yu,lnx,key,blu)
    else
       CALL qnerror('Specifying cell bottom levels bl (ibedlevtyp=1) or flow link bottom levels blu (ibedlevtyp=2)',' ',' ')
       CALL qnerror('Change parameter ibedlevtyp in Various, Change Geometry Parameters',' ',' ')
       return
    endif
    call setbobs()
    s1 = max(s1,bl)
 endif
 End subroutine plusabs_flow
