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

! $Id: make_orthocenters.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/make_orthocenters.f90 $

! compose an orthogonal dual mesh (cell centers), while keeping the primary mesh (net nodes) fixed
subroutine make_orthocenters(dmaxnonortho,maxiter)
   use m_netw
   use m_flowgeom, only: xz, yz
   use unstruc_display, only: ncolhl
   use geometry_module, only: dcosphi
   use m_sferic, only: jsferic, jasfer3D
   use m_missing, only : dmiss, dxymis
   use gridoperations

   implicit none

   double precision,               intent(in)  :: dmaxnonortho !< maximum allowed non-orthogonality
   integer,                        intent(in)  :: maxiter      !< maximum number of iterations

   integer,          parameter                 :: N6 = 6       ! maximum polygon dimension

   integer,          dimension(N6)             :: nodelist
   double precision, dimension(N6)             :: xplist, yplist, xflist, yflist

   double precision, dimension(:), allocatable :: xc, yc       !< cell centers

   double precision                            :: SL, SM, xcr, ycr, crp

   double precision                            :: af, dmaxabscosphi, drmsabscosphi, dabscosphi

   integer                                     :: iter

   integer                                     :: i, ip1, ii, ic, ic1, j, ja3, k, kp1, L, N, jacros

   integer                                     :: ierror

   double precision, parameter                 :: dsigma = 0.95d0


   ierror = 1
   ic     = 0

   if ( nump.lt.1 ) goto 1234

   if ( netstat.ne.NETSTAT_OK ) call findcells(0)

!  allocate
   allocate(xc(nump), yc(nump))

   call readyy(' ', -1d0)
   call readyy('Computing orthocenters (press right mouse button to cancel)', 0d0)

!  compute the initial cell centers
   do iter=1,MAXITER
      dmaxabscosphi = 0d0
      drmsabscosphi = 0d0
      do ic=1,nump
         N = netcell(ic)%N
         if ( N.gt.N6 ) then
            call qnerror('make_orthocenters: N>N6', ' ', ' ')
            goto 1234
         end if

         do i=1,N
            ip1 = i+1; if ( ip1.gt.N ) ip1=ip1-N
            k   = netcell(ic)%nod(i)
            kp1 = netcell(ic)%nod(ip1)
            xplist(i) = xk(k)
            yplist(i) = yk(k)
   !        find the link connected to this node
            do j=0,N-1
               ii = i+j; if ( ii.gt.N ) ii=ii-N
               L = netcell(ic)%lin(ii)
               if ( ( kn(1,L).eq.k .and. kn(2,L).eq.kp1 ) .or. ( kn(1,L).eq.kp1 .and. kn(2,L).eq.k ) ) then
                  exit  ! found
               end if
            end do

            if ( lnn(L).eq.2 ) then
   !           internal link
               ic1 = lne(1,L)+lne(2,L)-ic

                xflist(i) = xz(ic1)
                yflist(i) = yz(ic1)
                dabscosphi = abs(dcosphi(xz(ic),yz(ic),xz(ic1),yz(ic1),xk(k),yk(k),xk(kp1),yk(kp1), jsferic, jasfer3D, dxymis))
                dmaxabscosphi = max(dmaxabscosphi, dabscosphi)
                drmsabscosphi = drmsabscosphi + dabscosphi**2
            else
   !           boundary link
               xflist(i) = 0.5d0*(xk(k)+xk(kp1))
               yflist(i) = 0.5d0*(yk(k)+yk(kp1))
            end if
         end do

         call comp_circumcenter(N, xplist, yplist, xflist, yflist, xc(ic), yc(ic))

!         call cirr(xc(ic),yc(ic),31)
      end do   ! do ic=1,nump

      drmsabscosphi = sqrt(drmsabscosphi / dble(max(nump,1)))

!     relaxation
      xz(1:nump) = xz(1:nump) + dsigma*(xc(1:nump)-xz(1:nump))
      yz(1:nump) = yz(1:nump) + dsigma*(yc(1:nump)-yz(1:nump))

!     check residual
      if ( drmsabscosphi.le.dmaxnonortho ) exit

!     output information
      af = dble(iter)/dble(MAXITER)
      call readyy('Computing orthocenters (press right mouse button to cancel)', af)
      WRITE(6,'(1H+"iter: ", I5, " max ortho: ", E10.4, " rms ortho: ", E10.4)') iter, dmaxabscosphi, drmsabscosphi

!     check for right mouse button
      call halt3(ja3)
      if ( ja3.eq.3 ) then
         ierror = 0
         goto 1234
      end if
   end do   ! do iter=1,MAXITER

   ierror = 0
1234 continue

   call readyy(' ', -1d0)

   if ( ierror.ne.0 ) then
!      call qnerror('make_orthocenters: error', ' ', ' ')
      if ( ic.gt.0 .and. ic.lt.nump ) call cirr(xc(ic),yc(ic),ncolhl)
   end if

!  deallocate
   if ( allocated(xc)       ) deallocate(xc, yc)

   return
end subroutine make_orthocenters
