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

! $Id: netw2curv.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/netw2curv.f90 $

!> generate curvi-linear grid from net, growing from (xp,yp)
subroutine netw2curv(xp,yp)
 use m_netw
 use m_grid
 use m_alloc
 use m_missing
 use unstruc_messages
 use gridoperations

 implicit none

 double precision                   :: xp, yp       !< coordinates of starting point

 integer                            :: ierr

 integer, dimension(:), allocatable :: ic, jc       ! indices (i,j) of the nodes

! integer, parameter                 :: IMISS = -999999

 integer                            :: in, link, iexit

 logical                            :: lremovelink

!---------------------------------------------------------
! get the cells
!---------------------------------------------------------
 call findcells(0)

!---------------------------------------------------------
! allocate and initialize indices arrays
!---------------------------------------------------------
 allocate(ic(numk), stat=ierr)
 call aerr('ic(numk)', ierr, numk)

 allocate(jc(numk), stat=ierr)
 call aerr('jc(numk)', ierr, numk)

 ic = IMISS
 jc = IMISS
 in = 0

maindo:do
    if ( nump.lt.1 ) exit maindo

! allocate and initialize cellmask array
    call realloc(cellmask, numP)

! allocate and initialize ijc array
    if ( allocated(ijc) ) deallocate(ijc)
    call realloc(ijc, (/ 3, 3 /), (/ 0, 0 /), fill=IMISS)

!---------------------------------------------------------
! assigns node-based indices (ic,jc)
!---------------------------------------------------------
    call assign_icjc(xp,yp, ic, jc, iexit)
    if (iexit.ne.1) exit maindo
!---------------------------------------------------------
! generate the curvi-grid
!---------------------------------------------------------
    call makecurvgrid(ic, jc)

!---------------------------------------------------------
! remove curvi-grid nodes from net
!    firstly, disable links that do not neighbor a
!       non-curvi-grid cell: set kn to 0
!    secondly, rely on setnodadm to remove the proper nodes
!---------------------------------------------------------
     do link=1,numL
        lremovelink = .false.
        if ( lnn(link).gt.0)   &
           lremovelink = ( cellmask(lne(1,link)).eq. -1 )
        if ( lnn(link).gt.1)   &
           lremovelink = lremovelink .and. ( cellmask(lne(2,link)).eq. -1 )
        if ( lremovelink) kn(1:2, link) = 0
     end do

    call setnodadm(0)
    exit
 end do maindo
!---------------------------------------------------------
! done, clean up
!---------------------------------------------------------
 deallocate(ic)
 deallocate(jc)

!set network status
netstat = NETSTAT_CELLS_DIRTY

end subroutine netw2curv
