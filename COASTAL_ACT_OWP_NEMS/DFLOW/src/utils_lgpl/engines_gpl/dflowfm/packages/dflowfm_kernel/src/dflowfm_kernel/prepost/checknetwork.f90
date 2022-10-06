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

! $Id: checknetwork.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/checknetwork.f90 $

!> Check network data for possible errors.
!! Netlink crossings are stored in linkcross, and can be shown through display menu.
subroutine checknetwork()

use network_data
use unstruc_colors
use m_alloc
use geometry_module, only: cross
use m_missing, only: dmiss
use m_sferic, only: jsferic

implicit none

integer, allocatable :: linkQueue(:), jaLinkVisited(:)
integer :: nLink = 0

integer :: k, k1, k2, ka, kb, lprog, L, LL, jacros, nSearchRange, ncrossmax
double precision :: sl, sm, xcr, ycr, crp, E, E1

! It's impossible to reallocate in recursive findLinks, so reserve sufficient space here.
allocate(linkQueue(1000))
allocate(jaLinkVisited(numl))

! Allocate/reset linkcross array
ncrossmax = max(1,int(numl*0.01))
if (allocated(linkcross)) deallocate(linkcross)
allocate(linkcross(2, ncrossmax))
linkcross  = 0
nlinkcross = 0

lprog = 0
nSearchRange = 3 !< For a given link, search at most three connected links ahead
E = 1E-6 ; E1 = 1-E

call readyy('Checking net link crossings', 0d0)
!! Check crossing links
do L=1,numl

    if (L>=lprog) then
        call readyy('Checking net link crossings', dble(L)/dble(numl))
        lprog = lprog + int(numl/100.0)
    end if
    K1 = kn(1,L)
    K2 = kn(2,L)
lr: do k=1,2
        linkQueue(1:nLink) = 0
        nLink = 0
        call findLinks(kn(k,L))
        do LL=1,nLink
            jaLinkVisited(linkQueue(LL)) = 0
            KA = KN(1,linkQueue(LL)) ; KB = KN(2,linkQueue(LL))
            ! If interfaces share same node, no further action:
            if (k1 == ka .or. k1 == kb .or. k2 == ka .or. k2 == kb ) cycle
            call cross(XK(K1), YK(K1), XK(K2), YK(K2), XK(KA), YK(KA), XK(KB), YK(KB), JACROS,SL,SM,XCR,YCR,CRP, jsferic, dmiss)
            IF (jacros == 1.and. SL > E .AND. SL < E1 .AND. SM > E .AND. SM < E1 ) THEN
                if (nlinkcross >= ncrossmax) then
                    ncrossmax = int(1.2*ncrossmax) + 1
                    call realloc(linkcross, (/ 2, ncrossmax /), fill=0)
                end if
                nlinkcross = nlinkcross+1
                linkcross(1,nlinkcross) = L
                linkcross(2,nlinkcross) = linkQueue(LL)
                EXIT lr
            END IF
        end do
    end do lr
end do

call readyy('Checking net link crossings', -1d0)
deallocate(linkQueue)
deallocate(jaLinkVisited)
contains

!> Finds the set of links connected to a specified node through a certain
!! maximum number of intermediate links (nSearchRange=3).
!!
!! This set is used to detect link crossings within only a small range
!! from each link (brute force approach O(numl*numl) would be too expensive.
recursive subroutine findLinks(k)
!use m_alloc
use network_data
implicit none
integer :: k
!integer, intent(inout) :: linkQueue(:)
integer :: L, LL, k2, nQmax
integer, save :: nSearchDepth = 0

if ( k.lt.1 .or. k.gt.numk ) return

if (nSearchDepth  >= nSearchRange) return

nQmax = size(linkQueue)

nSearchDepth = nSearchDepth+1
do L=1,nmk(k)
    LL = nod(k)%lin(L)
    if (LL <= 0) exit
    if (nLink > nQmax)  exit ! Impossible to realloc in this recursive subroutine
    if (jaLinkVisited(LL)==1) then ! Walk links only once.
        cycle
    else
        jaLinkVisited(LL)=1
    end if

    ! 1. Add current link
    nLink = nLink+1
    linkQueue(nLink) = LL

    ! 2. And check recursively any connected links
    if (kn(2,LL) == k) then
        k2 = kn(1,LL)
    else
        k2 = kn(2,LL)
    end if
    call findLinks(k2)
end do
nSearchDepth = nSearchDepth-1

end subroutine findLinks

end subroutine checknetwork
