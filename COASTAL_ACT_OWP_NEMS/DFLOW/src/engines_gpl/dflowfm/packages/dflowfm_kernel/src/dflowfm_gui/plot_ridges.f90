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

! $Id: plot_ridges.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/plot_ridges.f90 $

!> plot the ridges
subroutine plot_ridges(ierror)

   use m_samples
   use m_samples_refine
   use m_missing
   use geometry_module, only: dbdistance

   implicit none

   integer, intent(out) :: ierror   !< error (1) or not (0)

   integer :: i, j, ip

   double precision :: Dx, Dy, dum, Dh, x0, y0, x1, y1, x2, y2

   double precision, external :: comp_sampleDh

   ierror = 1

   if ( iHesstat.ne.iHesstat_OK ) goto 1234

!  plot ridge
   do i=1,MXSAM
      do j=1,MYSAM
!        compute sample mesh width
         Dh = comp_sampleDh(i,j)

         ip = i+(j-1)*MXSAM

         if ( abs(zss(5,i,j)).gt.0.5d0*Dh .or. zss(4,i,j).gt.-1d-8 .or. zss(5,i,j).eq.DMISS ) cycle

         Dx =  zss(3,i,j)
         Dy = -zss(2,i,j)
         dum = Dh/sqrt(Dx**2+Dy**2+1d-16)
         Dx = Dx*dum
         Dy = Dy*dum

         call setcol(204)

         x0 = xs(ip)+zss(2,i,j)*zss(5,i,j)
         y0 = ys(ip)+zss(3,i,j)*zss(5,i,j)
         x1 = min(max(x0-Dx,xs(ip)-0.5d0*Dh), xs(ip)+0.5*Dh)
         y1 = min(max(y0-Dy,ys(ip)-0.5d0*Dh), ys(ip)+0.5*Dh)
         x2 = min(max(x0+Dx,xs(ip)-0.5d0*Dh), xs(ip)+0.5*Dh)
         y2 = min(max(y0+Dy,ys(ip)-0.5d0*Dh), ys(ip)+0.5*Dh)

         call movabs(x1,y1)
         call lnabs(x2,y2)
      end do
   end do

!   call qnerror(' ', ' ', ' ')

   ierror = 0
1234 continue

   return
end subroutine plot_ridges
