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

! $Id: m_snappol.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/rest_f90/m_snappol.f90 $

!---------------------------------------------------------------
module m_snappol  ! intentionally a module (for assumed size)
use kdtree2Factory
implicit none
   contains

!> snap polygon to mesh
  subroutine snappol(Nin, Xin, Yin, dsep, itype, Nout, Xout, Yout, ipoLout, ierror)
     use m_polygon
     use m_missing
     use m_alloc
     use m_flowgeom
     use network_data, only: xk, yk, kn
     use sorting_algorithms, only: indexxi
     implicit none

     integer,                                     intent(in)  :: Nin          !< thin-dyke polyline size
     double precision, dimension(Nin),            intent(in)  :: Xin, Yin     !< dsep-separated thin-dyke polyline coordinates
     double precision,                            intent(in)  :: dsep         !< separator
     integer,                                     intent(in)  :: itype        !< netlinks (1: cross with dual link, 3: cross with netlink itself) or flowlinks(2)


     integer,                                     intent(out) :: Nout         !< output polygon size
     double precision, dimension(:), allocatable, intent(out) :: Xout, Yout   !< output polygon coordinates, dim(Nout)
     integer,          dimension(:), allocatable, intent(out) :: ipoLout      !< reference to input polyline (>0), seperator w.r.t. input polyline (0), dim(Nout)
     integer,                                     intent(out) :: ierror       !< error (1) or not (0)

     integer                                                  :: NumLinks, NDIM

     double precision, dimension(:), allocatable              :: dSL
     integer,          dimension(:), allocatable              :: iLink, ipol
     integer,          dimension(:), allocatable              :: ipolnr, indx

     integer                                                  :: i, ii, iL, ipL, ipolsec, k1, k2, L, numpols

     ierror = 1

     Nout = 0

!    save polygon
     call savepol()

!    allocate
     allocate(iLink(Lnx))
     iLink = 0
     allocate(ipol(Lnx))
     ipol = 0
     allocate(dSL(Lnx))
     dSL = 0d0
     allocate(ipolnr(Nin))
     ipolnr = 999

!    number the input polyline segments
     numpols = 0   ! polyline segment counter
     i       = 1   ! pointer in input array
     do while ( i.lt.Nin )
!       advance pointer to start of segment
        do while ( (xin(i).eq.dsep .or. yin(i).eq.dsep) )
           ipolnr(i) = 0
           i=i+1
           if ( i.gt.Nin ) exit
        end do

!       check for end of array
        if ( i.gt.Nin ) exit

!       mark this segment
        numpols = numpols+1
        do while ( xin(i).ne.dsep .and. yin(i).ne.dsep )
           ipolnr(i) = numpols
           i=i+1
           if ( i.gt.Nin ) exit
        end do

!       check for end of array
        if ( i.gt.Nin ) exit
     end do

!!    BEGIN DEBUG
!     do i=1,NPL
!        zpl(i) = dble(ipolnr(i))
!     end do
!     goto 1234
!!    END DEBUG

!    temporarily use output arrays to replace missing value
     call realloc(xout, Nin, keepExisting=.false.)
     call realloc(yout, Nin, keepExisting=.false.)

!    replace missing values
     do i=1,Nin
        xout(i) = xin(i)
        yout(i) = yin(i)
        if ( xin(i).eq.dsep .or. yin(i).eq.dsep ) then
           xout(i) = DMISS
           yout(i) = DMISS
        end if
     end do

!    snap polygon (note: xout and yout are temporary arrays)
     call find_crossed_links_kdtree2(treeglob,Nin,xout,yout,itype,Lnx,1,NumLinks,iLink, iPol, dSL, ierror)
     if ( ierror.ne.0 .or. NumLinks.eq.0 ) goto 1234

!    sort crossed flowlinks in increasing polyline order
     allocate (indx(numLinks))
     call indexxi(numLinks,iPol,indx)

!    increase polygon array
     call increasepol(3*NumLinks,0)

     ii=1   ! pointer in indx array, sorted in increasing polygon number (iPol)

     do ipL=1,numpols
!       fill polygon with sections
        i = 0

!       advance pointer
        do while ( ipolnr(iPol(indx(ii))).lt.ipL )
           ii=ii+1
        end do

        if ( ii.gt.Numlinks ) then  ! done
           exit
        end if

        do while ( ipolnr(iPol(indx(ii))).eq.ipL )
        !do iL=1,NumLinks
           iL = indx(ii)

           L = iLink(iL)
!          check for matching polygon section
           ipolsec = iPol(iL)
           if ( ipolsec.lt.1 .or. ipolsec.ge.Nin ) then  ! should not happen
              continue
              exit
           end if
           if ( ipolnr(ipolsec).eq.ipL .or. ipolnr(ipolsec+1).eq.ipL ) then
               !             find the netnodes
              if (itype == 1 .or. itype == 3) then
                  k1 = kn(1,L)
                  k2 = kn(2,L)
              else ! itype == 2: flowlinks
                 k1 = lncn(1,L)
                 k2 = lncn(2,L)
              end if

!             fill the polyline array
              i=i+1
              xpl(i) = xk(k1)
              ypl(i) = yk(k1)
              i=i+1
              xpl(i) = xk(k2)
              ypl(i) = yk(k2)
              i=i+1
              xpl(i) = DMISS
              ypl(i) = DMISS
           else   ! should not happen
              continue
           end if

           ii = ii+1

           if ( ii.gt.Numlinks ) exit  ! done
        end do
        NPL = i

!       merge polyline section parts
        call merge_polylines()

        if ( NPL.lt.2 ) cycle  ! no polyline section found

!       copy to output
        NDIM = Nout + NPL + 1 ! add one for seperator
        call realloc(Xout, NDIM, keepExisting=.true.)
        call realloc(Yout, NDIM, keepExisting=.true.)
        call realloc(ipoLout, NDIM, keepExisting=.true.)
        do i=1,NPL
           Xout(Nout+i) = xpl(i)
           Yout(Nout+i) = ypl(i)
           if ( xpl(i).eq.DMISS .or. xpl(i).eq.DMISS ) then
              Xout(Nout+i) = dsep
              Yout(Nout+i) = dsep
           end if
           ipoLout(Nout+i) = ipL
        end do
        Nout = Nout + NPL + 1 ! add one for seperator
        Xout(Nout) = dsep
        Yout(Nout) = dsep
        ipoLout(Nout)  = 0

        if ( ii.gt.NumLinks ) exit  ! done
     end do

     ierror = 0
1234 continue

     if ( allocated(ipolnr) ) deallocate(ipolnr)
     if ( allocated(iLink)  ) deallocate(iLink)
     if ( allocated(iPol)   ) deallocate(iPol)
     if ( allocated(dSL)    ) deallocate(dSL)

     call restorepol()

     return
  end subroutine snappol


!> snap point to flow node
  subroutine snappnt(Nin, xin, yin, dsep, Nout, xout, yout, ipoLout, ierror)
     use m_alloc
     use m_flowgeom, only: xz, yz
     use m_GlobalParameters, only: INDTP_ALL
     use MessageHandling, only: IdLen
     implicit none

     integer,                                     intent(in)  :: Nin          !< thin-dyke polyline size
     double precision, dimension(Nin)                         :: Xin, Yin     !< dsep-separated thin-dyke polyline coordinates

     double precision,                            intent(in)  :: dsep         !< missing value

     integer,                                     intent(out) :: Nout         !< output polygon size
     double precision, dimension(:), allocatable, intent(out) :: Xout, Yout   !< output polygon coordinates, dim(Nout)
     integer,          dimension(:), allocatable, intent(out) :: ipoLout      !< reference to input points (>0), no flownode found (0), dim(Nout)
     integer,                                     intent(out) :: ierror       !< error (1) or not (0)

     character(len=IdLen), dimension(:), allocatable          :: namobs

     integer,          dimension(:), allocatable              :: kobs

     integer                                                  :: i, k
     integer                                                  :: jakdtree = 0

     ierror = 1
     Nout   = 0

     if ( Nin.lt.1 ) goto 1234

!    allocate
     allocate(namobs(Nin))
     allocate(kobs(Nin))

     do i=1,Nin
        namobs(i) = ''
        kobs(i)   = 0
     end do

     call find_flownode(Nin, xin, yin, namobs, kobs, jakdtree, 1, INDTP_ALL)

!    copy to output
     Nout = Nin
     call realloc(xout, Nout, keepExisting=.false., fill=dsep)
     call realloc(yout, Nout, keepExisting=.false., fill=dsep)
     call realloc(ipoLout, Nout, keepExisting=.false., fill=0)

     do i=1,Nout
        k = kobs(i)
        if ( k.gt.0 ) then
           xout(i) = xz(k)
           yout(i) = yz(k)
           ipoLout(i) = i
        else
           xout(i) = dsep
           yout(i) = dsep
           ipoLout(i) = 0
        end if
     end do

     ierror = 0
1234 continue

!    deallocate
     if ( allocated(namobs) ) deallocate(namobs)
     if ( allocated(kobs)   ) deallocate(kobs)

     return
  end subroutine snappnt


!> snap polyline to mesh boundary
!>   2D only
  subroutine snapbnd(bndtype, Nin, Xin, Yin, dsep, Nout, Xout, Yout, ipoLout, ierror)
     use timespace_triangle
     use m_polygon
     use m_missing
     use network_data, only: kn, xk, yk, NumL, lne
     use m_flowparameters, only: izbndpos
     implicit none

     character(len=*),                              intent(in)  :: bndtype    !< boundary condition type

     integer,                                       intent(in)  :: Nin          !< polyline size
     double precision, dimension(Nin),              intent(in)  :: Xin, Yin     !< dsep-separated polyline coordinates
     double precision,                              intent(in)  :: dsep         !< separator

     integer,                                       intent(out) :: Nout         !< output polygon size
     double precision, dimension(:),   allocatable, intent(out) :: Xout, Yout   !< output polygon coordinates, dim(Nout)
     integer,          dimension(:),   allocatable, intent(out) :: ipoLout      !< reference to input polyline (>0), seperator w.r.t. input polyline (0), dim(Nout)
     integer,                                       intent(out) :: ierror       !< error (1) or not (0)

     double precision, dimension(:),   allocatable              :: xe, ye
     double precision, dimension(:,:), allocatable              :: xyen
     double precision, dimension(:),   allocatable              :: xdum, ydum

     integer,          dimension(:),   allocatable              :: kce, ke, ki, kcs
     integer,          dimension(:),   allocatable              :: idx

     double precision                                           :: wL, wR
     double precision                                           :: xm, ym, crpm, distanceStartPolygon

     double precision, dimension(4)                             :: xx, yy
     double precision                                           :: xzz, yzz, xci, yci, xce2, yce2

     integer                                                    :: ierr, mx1Dend, Nx, numpols, jamiss
     integer                                                    :: i, iend, j, k1, k2, k3, k4, kL, kR, L, m, num, NDIM
     integer                                                    :: ja, isec, numsec, k, Lf

     integer            :: ioutput

     integer, parameter :: INETLINKS  = 0
     integer, parameter :: IFLOWNODES = 1
     integer, parameter :: IFLOWLINKS = 2

     select case(trim(bndtype))
        case( 'boundary' )
           ioutput = INETLINKS
        case( 'velocitybnd', 'dischargebnd', '1d2dbnd')
           ioutput = IFLOWLINKS
        case DEFAULT
           ioutput = IFLOWNODES
     end select

     ierror = 1
     Nout   = 0

!    save polygon
     call savepol()


!    count number of 2D links and 1D endpoints
     call count_links(mx1Dend, Nx)

!    allocate
     if(allocated(xe)) deallocate(xe, stat=ierror)
     if(allocated(ye)) deallocate(ye, stat=ierror)
     if(allocated(xyen)) deallocate(xyen, stat=ierror) 	
     if(allocated(kce)) deallocate(kce, stat=ierror)
     if(allocated(ke)) deallocate(ke, stat=ierror)
     if(allocated(ki)) deallocate(ki, stat=ierror)
     if(allocated(kcs)) deallocate(kcs, stat=ierror)
     if(allocated(xdum)) deallocate(xdum, stat=ierror)
     if(allocated(ydum)) deallocate(ydum, stat=ierror)

     allocate(xe(Nx), stat=ierror)
     allocate(ye(Nx), stat=ierror)
     allocate(xyen(2,Nx), stat=ierror)
     allocate(kce(Nx), stat=ierror)
     allocate(ke(Nx), stat=ierror)
     allocate(ki(Nx), stat=ierror)

     allocate(kcs(Nin), stat=ierror)
     allocate(xdum(Nin), stat=ierror)
     allocate(ydum(Nin), stat=ierror)

     kce = 0
     ke  = 0

!    replace missing values
     do i=1,Nin
        xdum(i) = xin(i)
        ydum(i) = yin(i)
        if ( xin(i).eq.dsep .or. yin(i).eq.dsep ) then
           xdum(i) = DMISS
           ydum(i) = DMISS
        end if
     end do

!    make mirror cells (will set kce and ke)
     call make_mirrorcells(Nx, xe, ye, xyen, kce, ke, ierror)

!    set polyline mask
     kcs = 1

!    loop over the input polylines
     numpols = 0   ! polyline counter
     i       = 1   ! pointer in input array
     do while ( i.lt.Nin )
!       advance pointer to start of segment
        do while ( (xin(i).eq.dsep .or. yin(i).eq.dsep) )
           i=i+1
           if ( i.gt.Nin ) exit
        end do

!       check for end of array
        if ( i.gt.Nin ) exit

!       find end pointer of this polyline
        numpols = numpols+1
        iend = i
        do while ( xin(iend).ne.dsep .and. yin(iend).ne.dsep )
           iend=iend+1
           if ( iend.gt.Nin ) exit
        end do
        iend = iend-1

!-------------------------------------------------------------------------------------------------------------------------
!       the core of this subroutine (mostly copied from selectelset)
!-------------------------------------------------------------------------------------------------------------------------
!       find boundary links
        num = 0
        do m = 1,Nx
           if (iabs(kce(m)) == 1) then     ! point is a possible candidate for a line boundary
              call polyindexweight(xe(m), ye(m),  xyen(1,m), xyen(2,m), Xdum(i:iend), Ydum(i:iend), kcs(i:iend), iend-i+1, kL, wL, kR, wR)
              ! if k1 > 0 this point can be dataprovided by this polyline
              if (kL > 0 .or. kR > 0) then
                 if ( kce(m) .eq. -1 ) then
                    !errormessage = 'Boundary location already claimed; Overlap with other bnds?'
                    !return
                    continue
                    cycle
                 else
                    num     =  num + 1
                    ki(num) =  m
                    kce(m)   = -1                ! this tells you this point is already claimed by some bnd
                 endif
              endif
           endif
        enddo
!-------------------------------------------------------------------------------------------------------------------------

!       copy found net links to polygon segments
        call increasepol(3*num, 0)
        NPL = 0
        do m=1,num
           L = ki(m)
           k1 = kn(1,L)
           k2 = kn(2,L)

           NPL = NPL+1
           XPL(NPL) = xk(k1)
           YPL(NPL) = yk(k1)

           NPL = NPL+1
           XPL(NPL) = xk(k2)
           YPL(NPL) = yk(k2)

           NPL = NPL+1
           XPL(NPL) = DMISS
           YPL(NPL) = DMISS
        end do

   !    merge polyline section parts
        call merge_polylines()

        if ( ioutput.eq.IFLOWNODES .or. ioutput.eq.IFLOWLINKS ) then
   !       find flownodes or flowlinks for output
           call realloc(idx, NPL, keepExisting=.false., fill=0)
           numsec = 0
           do i=1,num
              m = ki(i)
   !          find polyline section (again)
              call CROSSPOLY(xe(m),ye(m),xyen(1,m),xyen(2,m),XPL,YPL,NPL,xm,ym,crpm,ja,isec,distanceStartPolygon)

   !          remember which polyline segment points to this link
              if ( isec.gt.0 ) then
                 if ( idx(isec).eq.0 ) then
                    numsec = numsec+1
                    idx(isec) = m
                 else   ! should not happen
                    continue
                 end if
              else   ! should not happen
                 continue
              end if
           end do
        end if


!-------------------------------------------------------------------------------------------------------------------------
   !    copy to output
        NDIM = Nout + NPL + 1 ! add one for seperator
        call realloc(Xout, NDIM, keepExisting=.true., fill = DMISS)
        call realloc(Yout, NDIM, keepExisting=.true., fill = DMISS)
        call realloc(ipoLout, NDIM, keepExisting=.true., fill = 0)

        if ( ioutput.eq.INETLINKS ) then
        do m=1,NPL
           Xout(Nout+m) = xpl(m)
           Yout(Nout+m) = ypl(m)
           ipoLout(Nout+m) = numpols
        end do
        else if ( ioutput.eq.IFLOWLINKS ) then
           num = 0   ! polyline size
           jamiss = 0
           do i=1,NPL
              Xout(Nout+i) = DMISS
              Yout(Nout+i) = DMISS
              ipoLout(Nout+i) = numpols

              L = idx(i)
              if ( L.gt.0 .and. L.le.numL ) then
                 jamiss = 1
                 k1 = kn(1,L)
                 k2 = kn(2,L)
                 if ( k1.gt.0 .and. k2.gt.0 ) then
                    num = num+1
                    Xout(Nout+num) = 0.5d0*(xk(k1)+xk(k2))
                    Yout(Nout+num) = 0.5d0*(yk(k1)+yk(k2))
                    ipoLout(Nout+num) = numpols
                 end if
              else
                 if ( jamiss.eq.1 ) then ! add seperator
                    num = num+1
                    Xout(Nout+num) = DMISS
                    Yout(Nout+num) = DMISS
                    ipolout(Nout+num) = numpols
                    jamiss = 0
                 end if
              end if
           end do
           NPL = num
        else if ( ioutput.eq.IFLOWNODES ) then
           num = 0
           jamiss = 0
           do i=1,NPL
              m = idx(i)
              if ( m.gt.0 ) then
!-------------------------------------------------------------------------------------------------------------------------
! mostly copied from addexternalboundarypoints
!-------------------------------------------------------------------------------------------------------------------------
                 !if (kn(3,L) .ne. 1) then  ! in 2D mirror cell
                 if ( m.le.numL ) then
                    jamiss = 1
                    L  = m
                    k2 = iabs(lne(1,L))
                    k3 = kn(1,L); k4 = kn(2,L)

                    call mirrorcell(k2, xk(k3), yk(k3), xk(k4), yk(k4), xci, yci, xzz, yzz, xce2, yce2, xx, yy)
                    if (izbndpos == 0) then                    ! as in D3DFLOW

                    else if (izbndpos == 1) then               ! on network boundary
                       xzz  = 0.5d0*( xk(k3) + xk(k4 ) )
                       yzz  = 0.5d0*( yk(k3) + yk(k4 ) )
                    else if (izbndpos == 2) then               ! on specified boundary polyline

                    end if
                    num = num+1
                    Xout(Nout+num) = xzz
                    Yout(Nout+num) = yzz
                    ipoLout(Nout+num) = numpols
                 endif
!-------------------------------------------------------------------------------------------------------------------------
              else
                 if ( jamiss.eq.1 ) then
                    num = num+1
                    Xout(Nout+num) = DMISS
                    Yout(Nout+num) = DMISS
                    ipoLout(Nout+num) = numpols
                    jamiss = 0
                 end if
              end if
           end do
           NPL = num
        end if

!       remove trailing missing values
        if ( NPL.gt.0 ) then
           do while ( Xout(Nout+NPL).eq.DMISS .and. YOUT(Nout+NPL).eq.DMISS .and. NPL.gt.0)
              NPL = NPL-1
              if ( NPL.lt.1 ) exit
           end do
        end if

        if ( NPL.gt.0 ) then
        Nout = Nout + NPL + 1 ! add one for seperator
           Xout(Nout) = DMISS
           Yout(Nout) = DMISS
        ipoLout(Nout)  = 0
        end if

!       advance pointer
        i = iend+1

!       check for end of array
        if ( i.gt.Nin ) exit
     end do

!    replace DMISS with seperator, if necessary
     if ( dsep.ne.DMISS ) then
        do i=1,Nout
           if ( Xout(i).eq.DMISS .or. Yout(m).eq.DMISS ) then
              Xout(i) = dsep
              Yout(i) = dsep
           end if
        end do
     end if

!    trim output arrays to actual size
     call realloc(Xout,    Nout, keepExisting=.true.)
     call realloc(Yout,    Nout, keepExisting=.true.)
     call realloc(ipolout, Nout, keepExisting=.true.)

     ierror = 0
1234 continue

!    deallocate
     if ( allocated(xe)   ) deallocate(xe)
     if ( allocated(ye)   ) deallocate(ye)
     if ( allocated(xyen) ) deallocate(xyen)
     if ( allocated(kce)  ) deallocate(kce)
     if ( allocated(ke)   ) deallocate(ke)
     if ( allocated(ki)   ) deallocate(ki)
     if ( allocated(kcs)  ) deallocate(kcs)
     if ( allocated(xdum) ) deallocate(xdum)
     if ( allocated(ydum) ) deallocate(ydum)
     if ( allocated(idx)  ) deallocate(idx)

     call restorepol()

     return
  end subroutine snapbnd
end module m_snappol
