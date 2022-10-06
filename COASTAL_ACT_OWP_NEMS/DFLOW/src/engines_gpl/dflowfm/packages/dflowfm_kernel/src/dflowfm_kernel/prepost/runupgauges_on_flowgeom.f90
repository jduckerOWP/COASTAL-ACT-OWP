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

! $Id: runupgauges_on_flowgeom.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/runupgauges_on_flowgeom.f90 $

subroutine runupgauges_on_flowgeom()
    use m_monitoring_runupgauges
    use m_flowgeom, only: Lnx, lne2ln
    use m_missing
    use kdtree2Factory
    use unstruc_messages
    use dfm_error
    use m_inquire_flowgeom
    use m_partitioninfo, only: jampi
    use m_alloc
    implicit none

    integer                                       :: ic, icmod

    double precision, dimension(:),   allocatable :: xx, yy
    double precision, dimension(:),   allocatable :: dSL
    integer,          dimension(:),   allocatable :: iLink, ipol, istartcrs, numlist
    integer,          dimension(:,:), allocatable :: linklist
    integer,          dimension(:),   allocatable :: idum

    integer                                       :: i, num, numcrossedlinks, ierror
    integer                                       :: istart, iend

    integer                                       :: jakdtree=1
    double precision                              :: t0, t1
    character(len=128)                            :: mesg
    integer                                       :: linknr, ii
    logical                                       :: success


    if ( nrug.lt.1 ) return

    numcrossedlinks = 0

!   allocate
    allocate(istartcrs(nrug+1))
    istartcrs = 1

    allocate(idum(1))
    idum = 0

    if ( jakdtree.eq.1 ) then
        call klok(t0)

        ! to do: chaching
        !call copyCachedCrossSections( iLink, ipol, success )

        !if ( success ) then
        !    numcrossedlinks = size(iLink)
        !    ierror          = 0
        !else
            num = 0
!           determine polyline size
            do ic=1,nrug
               num = num+rug(ic)%path%np+1 ! add space for missing value
               istartcrs(ic+1) = num+1
            end do

!           allocate
            allocate(xx(num), yy(num))

!           determine paths to single polyline map
            num = 0
            do ic=1,nrug
               do i=1,rug(ic)%path%np
                  num = num+1
                  xx(num) = rug(ic)%path%xp(i)
                  yy(num) = rug(ic)%path%yp(i)
               end do
!              add missing value
               num = num+1
               xx(num) = DMISS
               yy(num) = DMISS
            end do

!           allocate
            allocate(iLink(Lnx))
            iLink = 0
            allocate(ipol(Lnx))
            ipol = 0
            allocate(dSL(Lnx))
            dSL = 0d0
            ! use itype 3, as we want crossing the edge, not the connection between adjoint cells
            call find_crossed_links_kdtree2(treeglob,num,xx,yy,3,Lnx,1,numcrossedlinks, iLink, ipol, dSL, ierror)

            !call saveLinklist( numcrossedlinks, iLink, ipol )   to do caching
        !endif

        if ( ierror.eq.0 .and. numcrossedlinks.gt.0 ) then

!          determine crossed links per cross-section
           allocate(numlist(nrug))
           numlist = 0
           allocate(linklist(numcrossedlinks,nrug))
           linklist = 0

           do i=1,numcrossedlinks
              do ic=1,nrug
                 istart  = istartcrs(ic)
                 iend    = istartcrs(ic+1)-1
                 if ( ipol(i).ge.istart .and. ipol(i).le.iend ) then
                    numlist(ic) = numlist(ic)+1
                    linklist(numlist(ic),ic) = iabs(lne2ln(iLink(i)))
                 end if
              end do
           end do

        else
!          disable kdtree
           jakdtree = 0

!          deallocate
           if ( allocated(iLink) ) deallocate(iLink)
           if ( allocated(ipol)  ) deallocate(ipol)
           if ( allocated(dSL)   ) deallocate(dSL)
        end if

!       deallocate
        if ( allocated(istartcrs) ) deallocate(istartcrs)
        if ( allocated(xx)        ) deallocate(xx,yy)

        call klok(t1)
        write(mesg,"('runup gauges with kdtree2, elapsed time: ', G15.5, 's.')") t1-t0
        call mess(LEVEL_INFO, trim(mesg))
    end if

    icMOD = MAX(1,nrug/100)

    call realloc(numlist, nrug, keepExisting = .true., fill = 0) ! In case pli-based cross sections have not allocated this yet.
    call realloc(linklist, (/ max(numcrossedlinks, 1), nrug /), keepExisting = .true., fill = 0)  ! In addition to pli-based cross sections (if any), also support 1D branchid-based cross sections.

    ! todo: caching
    !call copyCachedCrossSections( iLink, ipol, success )

    CALL READYY('Enabling runup gauges on grid', 0d0)
    do ic=1,nrug
        if (mod(ic,icMOD) == 0) then
            CALL READYY('Enabling runup gauges on grid', dble(ic)/dble(nrug))
        end if
        !
        !if ( .not. success ) then   to do: caching
           if ( jakdtree.eq.0 ) then
              call crspath_on_flowgeom(rug(ic)%path,0,0,1,idum, 0, 2)
           else
              call crspath_on_flowgeom(rug(ic)%path,0,1,numlist(ic),linklist(1,ic), 0, 2)
           end if
        !end if
    end do

    CALL READYY('Enabling runup gauges on grid', -1d0)

1234 continue

!   deallocate
    if ( jakdtree.eq.1 ) then
       if ( allocated(iLink)    ) deallocate(iLink)
       if ( allocated(iPol)     ) deallocate(iPol)
       if ( allocated(dSL)      ) deallocate(dSL)
       if ( allocated(numlist)  ) deallocate(numlist)
       if ( allocated(linklist) ) deallocate(linklist)
    endif

    if ( allocated(idum)     ) deallocate(idum)

   return
end subroutine runupgauges_on_flowgeom
