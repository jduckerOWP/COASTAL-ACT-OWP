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

! $Id: fliplinks.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/fliplinks.f90 $

!> flip links in quads, when appropriate
!>   note: we look for a local optimum, which is not necessarily the global one
subroutine fliplinks()
   use m_netw
   use m_alloc
   use unstruc_colors, only: ncolhl
   use m_orthosettings, only: japroject
   use geometry_module, only: cross
   use m_sferic, only: jsferic
   use m_missing, only: dmiss
   use gridoperations

   implicit none

   integer                              :: L                 ! link number

   integer, allocatable, dimension(:)   :: inodemask         ! node mask

   integer                              :: k1, k2, kL, kR
   integer                              :: icellL, icellR
   integer                              :: k, kk, LL

   integer                              :: x1, x2, xL, xR    ! deviation from optimal nmk

   logical                              :: Lproceed

   integer                              :: ntopo             ! change in topology functional

!   integer,              dimension(4)   :: nmk_opt           ! optimal nmk for the for nodes involved

   integer                              :: numchanged        ! number of linkes flipped

   integer                              :: iter              ! iteration
   integer                              :: MAXITER           ! maximum number of iterations

   integer                              :: L1L, L1R, L2L, L2R ! other links in triangles connected to link L

   integer                              :: jacross           ! check if two diagonals of a quadrilateral cross

   integer                              :: irerun

   integer                              :: jatriangulate     ! triangulate all cells prior to link flippingz
   integer                              :: jalandbound       ! take land boundaries into account or not

   integer                              :: maxlin
   double precision, allocatable        :: arglin(:)         ! dummy array
   integer, allocatable                 :: linnrs(:), inn(:) ! dummy arrays

   double precision                     :: sl, sm, xcr, ycr, crp ! used in cross check

   double precision                     :: beta, Etot, Emin  ! Monte-Carlo parameters

   logical                              :: Lflip

   integer,          external           :: nmk_opt

   double precision, external           :: rand

   integer                              :: ja

   integer                              :: lunfil


   if ( jaswan.ne.1 ) then

      jatriangulate = 1
      call confrm('triangulate all cells prior to link flipping?', jatriangulate)

      jalandbound = 1
      call confrm('take land boundaries into account?', jalandbound)

   else
      jatriangulate = 1
      jalandbound   = 0
      if ( japroject.eq.3 .or. japroject.eq.4 ) jalandbound = 1
   end if

   call findcells(100)
   call makenetnodescoding()

   if ( jatriangulate.eq.1 ) then
      call triangulate_cells()

      call findcells(100)
      call makenetnodescoding()
   end if

   if ( jalandbound .eq. 1 ) then
      call find_nearest_meshline(4)
   end if



!  Monte-Carlo settings
   MAXITER = 10
   beta    = 2d0
   Etot    = 0d0
   Emin    = Etot


!  allocate
   allocate(inodemask(numk))
   maxlin = maxval(nmk(1:numk)) + 10   ! safety
   allocate(linnrs(maxlin),arglin(maxlin),inn(maxlin))

!   open(newunit=lunfil, file='test.m')
!   write(lunfil, "('data=[')")

it:do iter=1,MAXITER
      inodemask  = 0
      numchanged = 0

      do L=1,numL

         call comp_ntopo(L, jalandbound, k1, k2, kL, kR, icellL, icellR, ntopo)

      !  check and see if the nodes are masked
         if ( inodemask(k1).ne.0 .or. inodemask(k2).ne.0  ) cycle

         if ( lnn(L).ne.2 ) cycle  ! inner links only
!
         if ( netcell(icellL)%N.ne.3 .or. netcell(icellR)%N.ne.3 ) cycle  ! triangles only

      !  check and see if the nodes are masked
         if ( inodemask(kL).ne.0 .or. inodemask(kR).ne.0 ) cycle

         Lflip = ( ntopo.lt.0 )

!          Monte-Carlo
!          if( abs(beta*dble(ntopo)).lt.5d0 ) then
!            Lflip = ( rand(0).lt.dexp(-beta*dble(ntopo)) )
!          else
!            Lflip = ( ntopo.lt.0 )
!          end if

          if ( Lflip ) then
      !     whipe out link
            call teklink(L, 0)

      !     check if the quadrilateral composed by the two adjacent triangles is concave,
      !       in which case the diagonals cross
            call cross(xk(k1), yk(k1), xk(k2), yk(k2), xk(kL), yk(kL), xk(kR), yk(kR), jacross, sl, sm, xcr, ycr, crp, jsferic, dmiss)

            if ( jacross.eq.0 ) then ! concave: mesh fold ahead
               cycle
            end if

!           Monte-Carlo: modify total energy
!            Etot = Etot + ntopo

      !     flip link
            kn(1,L) = kL
            kn(2,L) = kR

      !     mask nodes
!            inodemask(k1) = 1
!            inodemask(k2) = 1
!            inodemask(kL) = 1
!            inodemask(kR) = 1
            numchanged = numchanged+1

       !    find the other links
            do kk=1,netcell(icellL)%N
               LL = netcell(icellL)%lin(kk)
               if ( LL.eq.L ) cycle
               if ( kn(1,LL).eq.k1 .or. kn(2,LL).eq.k1 ) L1L = LL
               if ( kn(1,LL).eq.k2 .or. kn(2,LL).eq.k2 ) L2L = LL
            end do

            do kk=1,netcell(icellR)%N
               LL = netcell(icellR)%lin(kk)
               if ( LL.eq.L ) cycle
               if ( kn(1,LL).eq.k1 .or. kn(2,LL).eq.k1 ) L1R = LL
               if ( kn(1,LL).eq.k2 .or. kn(2,LL).eq.k2 ) L2R = LL
            end do

       !    change cells
       !      orientation, i.e. clockwise vs. counterclockwise, not sorted out here
       !      tiangles only
            netcell(icellL)%nod(1:3) = (/ kL,  kR, k1 /)
            netcell(icellL)%lin(1:3) = (/ L,  L1R, L1L /)
            netcell(icellR)%nod(1:3) = (/ kL,  kR, k2 /)
            netcell(icellR)%lin(1:3) = (/ L,  L2R, L2L /)

            if ( lne(1,L1R).eq.icellR ) then
               lne(1,L1R) = icellL
            else
               lne(2,L1R) = icellL
            end if

            if ( lne(1,L2L).eq.icellL ) then
               lne(1,L2L) = icellR
            else
               lne(2,L2L) = icellR
            end if

       !    update nmk
            nmk(k1) = nmk(k1) - 1
            nmk(k2) = nmk(k2) - 1
            nmk(kL) = nmk(kL) + 1
            nmk(kR) = nmk(kR) + 1

       !    update nod
       !    delete link from nod(k1)
            kk=1; do while( nod(k1)%lin(kk).ne.L .and. kk.le.nmk(k1) ); kk=kk+1; end do
            if ( nod(k1)%lin(kk).ne.L ) goto 1234
            nod(k1)%lin(1:nmk(k1)) = (/ nod(k1)%lin(1:kk-1), nod(k1)%lin(kk+1:nmk(k1)+1) /)
            call realloc(nod(k1)%lin,nmk(k1))

       !    delete link from nod(k2)
            kk=1; do while( nod(k2)%lin(kk).ne.L .and. kk.le.nmk(k2) ); kk=kk+1; end do
            if ( nod(k2)%lin(kk).ne.L ) goto 1234
            nod(k2)%lin(1:nmk(k2)) = (/ nod(k2)%lin(1:kk-1), nod(k2)%lin(kk+1:nmk(k2)+1) /)
            call realloc(nod(k2)%lin,nmk(k2))

       !    add link to nod(kL)
            call realloc(nod(kL)%lin,nmk(kL))
            nod(kL)%lin = (/ nod(kL)%lin(1:nmk(kL)-1), L /)
            call sort_links_ccw(kL,maxlin,linnrs,arglin,inn)

       !    add link to nod(kR)
            call realloc(nod(kR)%lin,nmk(kR))
            nod(kR)%lin = (/ nod(kR)%lin(1:nmk(kR)-1), L /)
            call sort_links_ccw(kR,maxlin,linnrs,arglin,inn)

        !   highlight new link
            call teklink(L, ncolhl)

!            ja = 1
!            call confrm('continue', ja)
!            if ( ja.eq.0 ) then
!               exit it
!            end if
         end if
      end do

!      write(lunfil,*)  iter, numchanged, Etot, Emin

!      if ( numchanged.eq.0 ) exit   ! done

!     Monte-Carlo
!      if ( mod(iter, 1000) .eq. 0) then
!         call orthogonalisenet(irerun)
!         beta = beta * 1.1d0
!      end if
!      if ( Etot.lt.Emin ) then   ! new minimum
!         call setnodadm(0)
!         call SAVENET()
!         Emin = Etot
!      end if

   end do it

!  Monte-Carlo: minimum
!   if ( Etot.gt.Emin) then
!      call restore()
!   end if

   if ( numchanged.ne.0 ) then   ! not converged
      call qnerror('fliplinks: not converged', ' ', ' ')
   end if

1234 continue  ! error handling

!  deallocate
   if ( allocated(inodemask) ) deallocate(inodemask)
   if ( allocated(linnrs) )    deallocate(linnrs,arglin,inn)
!   write(lunfil, "('];')")
!   close(lunfil)

!  update administration
   call findcells(100) ! also find folded cells
   call makenetnodescoding()

   return

end subroutine fliplinks
