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

! $Id: create_samples_in_triangle.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/rest_f90/create_samples_in_triangle.f90 $

!> create samples in triangle
subroutine create_samples_in_triangle()

   use m_polygon
   use m_samples
   use network_data, only: cornercos
   use m_missing
   use m_sferic, only: jsferic, jasfer3D
   use geometry_module, only: dbdistance, dcosphi

   implicit none

   double precision, dimension(:,:), allocatable :: xx
   double precision, dimension(:,:), allocatable :: yy

   double precision                              :: dcos
   double precision                              :: xi, eta
   double precision                              :: dfac, dfacL, dfacR, RL, RR

   integer                                       :: n1, n2, n3
   integer                                       :: M, N, Nxi, Msize
   integer                                       :: i, j
   integer                                       :: jL, jR


   if ( NPL.lt.2 ) return

!  delete samples
   call delsam(-1)

!  find startpoint of triangle in polygon
   n1 = 1
   do while ( xpl(n1).eq.DMISS )
      if ( n1.eq.NPL ) exit
      n1 = n1+1
   end do
   if ( xpl(n1).eq.DMISS ) goto 1234

!  find first corner
   n2 = n1+1
   if ( n2.ge.NPL ) goto 1234

   dcos = dcosphi(xpl(n2-1),ypl(n2-1),xpl(n2),ypl(n2),xpl(n2),ypl(n2),xpl(n2+1),ypl(n2+1), jsferic, jasfer3D, dxymis)
   do while ( dcos.gt.cornercos )
      n2 = n2+1
      if ( n2.eq.NPL ) goto 1234
      dcos = dcosphi(xpl(n2-1),ypl(n2-1),xpl(n2),ypl(n2),xpl(n2),ypl(n2),xpl(n2+1),ypl(n2+1), jsferic, jasfer3D, dxymis)
   end do
   if ( dcos.gt.cornercos ) goto 1234

!  find third corner
   n3 = n2+1
   if ( n3.ge.NPL ) goto 1234

   dcos = dcosphi(xpl(n3-1),ypl(n3-1),xpl(n3),ypl(n3),xpl(n3),ypl(n3),xpl(n3+1),ypl(n3+1), jsferic, jasfer3D, dxymis)
   do while ( dcos.gt.cornercos )
      n3 = n3+1
      if ( n3.eq.NPL ) exit
      dcos =  dcosphi(xpl(n3-1),ypl(n3-1),xpl(n3),ypl(n3),xpl(n3),ypl(n3),xpl(n3+1),ypl(n3+1), jsferic, jasfer3D, dxymis)
   end do

!  determine dimensions
   M = n2-n1+1
   N = n3-n2+1

   if ( n3+M-1.gt.NPL ) goto 1234

   Msize = max(M,N)
   allocate(xx(Msize,3))
   xx = 0d0
   allocate(yy(Msize,3))
   yy = 0d0
   do i=1,M
      xx(i,1) = xpl(n1+i-1)-xpl(n1)
      yy(i,1) = ypl(n1+i-1)-ypl(n1)
      xx(i,3) = xpl(n3+M-i)-xpl(n1)
      yy(i,3) = ypl(n3+M-i)-ypl(n1)
   end do

   do j=1,N
      xx(j,2) = xpl(n2+j-1)-xpl(n1)
      yy(j,2) = ypl(n2+j-1)-ypl(n1)
   end do

   call increasesam(M*N)

   Ns=Ns+1
   xs(1) = xpl(n1)
   ys(1) = ypl(n1)

   RL = dbdistance(xpl(n1),ypl(n1),xpl(n2),ypl(n2),jsferic, jasfer3D, dmiss)
   RR = dbdistance(xpl(n1),ypl(n1),xpl(n3),ypl(n3),jsferic, jasfer3D, dmiss)

   do i=2,M-1

      xi = dble(i-1)/dble(M-1)
      Nxi = floor(xi*(N-1)+1)

      dfacL = dbdistance(xpl(n1),ypl(n1),xpl(n1)+xx(i,1),ypl(n1)+yy(i,1),jsferic, jasfer3D, dmiss)/RL
      dfacR = dbdistance(xpl(n1),ypl(n1),xpl(n1)+xx(i,3),ypl(n1)+yy(i,3),jsferic, jasfer3D, dmiss)/RR

      do j=2,Nxi-1
         eta = dble(j-1)/dble(Nxi-1)

         jL = 1 + floor(eta*(N-1))
         if ( jL.ge.N ) jL=N-1
         jR = jL+1

         dfac = 1d0+eta*(N-1)-jL

         Ns = Ns+1
!         xs(Ns) = (1d0-xi)*xpl(n1) + xi*( (1-eta)*xpl(n2) + eta*xpl(n3) )
!         ys(Ns) = (1d0-xi)*ypl(n1) + xi*( (1-eta)*ypl(n2) + eta*ypl(n3) )
!         xs(Ns) = xpl(n1) + xi*((1-dfac)*xx(jL,2) + dfac*xx(jR,2))
!         ys(Ns) = ypl(n1) + xi*((1-dfac)*yy(jL,2) + dfac*yy(jR,2))

!         xs(Ns) = (1-dfac) * xpl(n1) + dfac*xs(Ns)
!         ys(Ns) = (1-dfac) * ypl(n1) + dfac*ys(Ns)

         xs(Ns) = xpl(n1) + (1-dfac)*xx(jL,2) + dfac*xx(jR,2)
         ys(Ns) = ypl(n1) + (1-dfac)*yy(jL,2) + dfac*yy(jR,2)

         dfac = (1-eta)*dfacL + eta*dfacR

         xs(Ns) = (1-dfac) * xpl(n1) + dfac * xs(Ns)
         ys(Ns) = (1-dfac) * ypl(n1) + dfac * ys(Ns)
      end do
      Ns = Ns+1
      xs(Ns) = xpl(n1)+xx(i,1)
      ys(Ns) = ypl(n1)+yy(i,1)
      Ns = Ns+1
      xs(Ns) = xpl(n1)+xx(i,3)
      ys(Ns) = ypl(n1)+yy(i,3)
   end do

   do j=1,N
      Ns = Ns+1
      xs(Ns) = xpl(n1)+xx(j,2)
      ys(Ns) = ypl(n1)+yy(j,2)
   end do

   do i=1,Ns
      zs = 0d0
   end do

1234 continue

!  deallocate
   if ( allocated(xx) ) deallocate(xx)
   if ( allocated(yy) ) deallocate(yy)

   return
end subroutine create_samples_in_triangle
