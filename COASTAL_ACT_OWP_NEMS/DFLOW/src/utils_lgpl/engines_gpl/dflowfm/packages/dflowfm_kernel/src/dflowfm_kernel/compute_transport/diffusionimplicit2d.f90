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

! $Id: diffusionimplicit2d.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_transport/diffusionimplicit2d.f90 $

subroutine diffusionimplicit2D()
use m_transport
use m_flowgeom
use m_flow
use m_flowtimes
use m_reduce
use timers

implicit none

double precision :: ddx, difcoeff, diuspL, diag
integer i, k1, k2, L, LL, n

integer(4) ithndl /0/
if (timon) call timstrt ( "diffusionimplicit2D", ithndl )

do i=1,numconst

   bbr = 0d0 ; ccr = 0d0
   do L=1,lnx
      if (dxiau(L) > 0d0) then
         k1 = ln(1,L) ; k2 = ln(2,L)
         if (jadiusp == 1) then
             diuspL = diusp(L)
         else
             diuspL = dicouv
         endif
         difcoeff    = sigdifi(i)*viu(L) + difsedu(i) + diuspL
         ddx         = dxiau(L)*max(0d0, difcoeff)  ! safety first...
         bbr(k1)     = bbr(k1)     + ddx
         bbr(k2)     = bbr(k2)     + ddx
         ccr(lv2(L)) = ccr(lv2(L)) - ddx
      endif
   enddo
   do n = 1,ndx
      if (bbr(n) > 0d0) then
         diag    = 0.5d0*( vol0(n) + vol1(n) )*dti  ! safety first...,  flooding : vol1 > 0, ebbing : vol0 > 0
         bbr(n)  = bbr(n) + diag
         ddr(n)  = diag*constituents(i,n)
      else
         bbr(n)  = 1d0
         ddr(n)  = constituents(i,n)
      endif
      workx(n) = constituents(i,n)
   enddo
   call solve_matrix(workx,ndx,itsol)
   do n = 1,ndxi
      constituents(i,n) = workx(n)
   enddo

enddo

if (timon) call timstop( ithndl )
end subroutine diffusionimplicit2D
