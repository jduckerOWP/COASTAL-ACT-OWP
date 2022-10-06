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

! $Id: find_crossed_links_kdtree2.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/rest_f90/find_crossed_links_kdtree2.f90 $

!---------------------------------------------------------------
! the following subroutines use kdtree2
!---------------------------------------------------------------
!> find links crossed by polyline with kdtree2
   subroutine find_crossed_links_kdtree2(treeinst,NPL,xpl,ypl,itype,nLinks,jaboundarylinks,numcrossedLinks, iLink, iPol, dSL, ierror)

      !use m_polygon
      use network_data, only: nump, numL, kn, xk, yk, lnn,lne
      use m_flowgeom
      use kdtree2Factory
      use m_sferic
      use unstruc_messages
      use m_missing
      use m_alloc
      use geometry_module, only: dbdistance, crossinbox

      implicit none

      type(kdtree_instance),               intent(inout) :: treeinst
      integer,                             intent(in)    :: NPL                !< polyline length
      double precision, dimension(NPL),    intent(in)    :: xpl, ypl           !< polyline node coordinates
      integer,                             intent(in)    :: itype              !< netlinks (1: cross with dual link, 3: cross with netlink itself) or flowlinks(2)
      integer,                             intent(in)    :: nLinks             !< number of links ( Lnx for flowlinks, numL for netlinks)
      integer,                             intent(in)    :: jaboundarylinks    !< include boundary links (1) or not (0), flowlinks only
      integer,                             intent(out)   :: numcrossedLinks    !< number of crossed flowlinks
      integer,          dimension(nLinks), intent(inout) :: iLink              !< crossed flowlinks
      integer,          dimension(nLinks), intent(inout) :: iPol               !< polygon section
      double precision, dimension(nLinks), intent(inout) :: dSL                !< polygon section cross location
      integer,                             intent(out)   :: ierror             !< ierror (1) or not (0)

      double precision, dimension(:),       allocatable  :: x, y

      integer,          dimension(:),       allocatable  :: ipolsection
      double precision                                   :: dmaxpollen, dmaxLinlen, dlinlen, R2search
      integer                                            :: num
      integer,                              parameter    :: jakdtree=1
      integer,                              parameter    :: MAXFIND=100
      integer,                              parameter    :: MINTREESIZE=0

      double precision                                   :: SL, SM, XCR, YCR, CRP
      double precision                                   :: xa, ya, xb, yb, af, d
      integer                                            :: i, k, L, N1, N2, NN, numnew
      integer                                            :: jacros, kint
      integer                                            :: LnxiORLnx
      integer                                            :: isactive
      double precision                                   :: dtol

      if ( janeedfix.eq.1 ) then
         dtol = 1d-8
      else
         dtol = 0d0
      end if

      ierror          = 1

      numcrossedLinks = 0

      if ( NPL.lt.1 ) goto 1234  ! nothing to do

      LnxiORLnx = 0

      if ( itype.eq.1 .or. itype.eq.3 ) then  ! netlinks
         LnxiORLnx = numL
      else if ( itype.eq.2 ) then   ! flowlinks
         if ( jaboundarylinks.eq.1 ) then
            LnxiORLnx = Lnx
         else
            LnxiORLnx = Lnxi
         end if
      end if

!     allocate
      allocate(ipolsection(NPL-1))
      allocate(treeinst%qv(NTREEDIM))

!     determine maximum polygon section length, and administer polygon sections
      dmaxpollen = 0d0
      num = 0
      do i=1,NPL-1
         if ( xpl(i).ne.DMISS .and. xpl(i+1).ne.DMISS ) then
            num = num+1
            ipolsection(num) = i
            dmaxpollen = max(dmaxpollen, dbdistance(xpl(i),ypl(i),xpl(i+1),ypl(i+1),jsferic, jasfer3D, dmiss))
         end if
      end do

!     check tree size and exit if the tree is too small
      if ( num.lt.MINTREESIZE ) then
         goto 1234
      end if

!     build kdtree
      allocate(treeinst%sample_coords(NTREEDIM,NPL-1))

      NN = min(MAXFIND,num)

!     allocate
      allocate(x(num), y(num))

!     fill coordinates
      if ( janeedfix.eq.1 ) then
         do k=1,num
            i=ipolsection(k)
            call random_number(d)
            x(k) = xpl(i) + dtol*d
            call random_number(d)
            y(k) = ypl(i) + dtol*d
         end do
      else
         do k=1,num
            i=ipolsection(k)
            x(k) = xpl(i)
            y(k) = ypl(i)
         end do
      end if

      call build_kdtree(treeinst,num, x, y, ierror, jsferic, dmiss)
      if ( ierror.ne.0 ) then
         goto 1234
      end if

!     find crossed flowlinks
      call mess(LEVEL_INFO, 'Finding crossed flowlinks...')

      kint = max(LnxiORLnx/1000,1)

      do L=1,LnxiORLnx
         if (mod(L,kint) == 0) then
            af = dble(L)/dble(LnxiORLnx)
            call readyy('Finding crossed links', af)
!            write(6,"(F4.1, ' %')") af*100d0
         endif


         if ( itype.eq.1 ) then   ! netlinks, cross with dual links
            call get_link_neighboringcellcoords(L,isactive,xa,ya,xb,yb)
            if ( isactive.ne.1 ) then
               cycle
            end if
         else if ( itype.eq.2 ) then ! flowlinks
            n1 = ln(1,L) ; n2 = ln(2,L)
            xa = xz(n1)  ; ya = yz(n1)
            xb = xz(n2)  ; yb = yz(n2)
         else if ( itype.eq.3 ) then   ! netlinks, cross with netlinks
            n1 = kn(1,L)
            n2 = kn(2,L)
            xa = xk(n1)
            ya = yk(n1)
            xb = xk(n2)
            yb = yk(n2)
         end if

!        fill query vector
         call make_queryvector_kdtree(treeinst,xa,ya, jsferic)

!        compute flowlink length
         dlinlen = dbdistance(xa,ya,xb,yb, jsferic, jasfer3D, dmiss)

!        determine square search radius
         if ( jsferic.eq.0 ) then
            R2search = 1.1d0*(dlinlen+dmaxpollen+2d0*dtol)**2  ! 1.1d0: safety
         else
            R2search = 1.1d0*(dlinlen+dmaxpollen+2d0*dtol*Ra)**2  ! 1.1d0: safety
         end if

!        count number of points in search area
         NN = kdtree2_r_count(treeinst%tree,treeinst%qv,R2search)

         if ( NN.eq.0 ) cycle ! no links found

!        reallocate if necessary
         call realloc_results_kdtree(treeinst,NN)

!        find nearest NN points
         call kdtree2_n_nearest(treeinst%tree,treeinst%qv,NN,treeinst%results)

         jacros = 0
         do i=1,NN
            k = ipolsection(treeinst%results(i)%idx)
            CALL crossinbox (XPL(k), YPL(k), XPL(k+1), YPL(k+1), Xa, Ya, Xb, Yb, jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)

            if ( jacros.eq.1 ) then
               numcrossedLinks = numcrossedLinks + 1

               if ( numcrossedLinks.gt.ubound(iLink,1) ) then
                  call mess(LEVEL_ERROR, 'find_crossed_links_kdtree2: array size too small')
               end if

               iLink(numcrossedLinks) = L
               iPol(numcrossedLinks)  = k
               dSL(numcrossedLinks)   = SL
            end if
         end do
      end do

      call readyy(' ', -1d0 )

      call mess(LEVEL_INFO, 'done')

      ierror = 0
 1234 continue

!     deallocate
      if ( treeinst%itreestat.ne.ITREE_EMPTY ) call delete_kdtree2(treeinst)
      if ( allocated(ipolsection) )   deallocate(ipolsection)
      if ( allocated(x) )             deallocate(x)
      if ( allocated(y) )             deallocate(y)

      return
   end subroutine find_crossed_links_kdtree2
