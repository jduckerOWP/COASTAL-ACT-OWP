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

! $Id: droptracer.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_transport/droptracer.f90 $

subroutine droptracer(xp, yp, dval)
   use m_transport
   use m_flowgeom
   use m_flow, only: kmxn, kbot
   use m_polygon
   use m_missing, only: dmiss, JINS
   use geometry_module, only: dbpinpol

   implicit none

   double precision :: xp, yp   !< point coordinates
   double precision :: dval     !< value

   integer, dimension(:), allocatable :: icelllist
   integer                            :: Ncells

   integer                            :: i, k, kk, kb, kt
   integer                            :: N, in
   integer                            :: ja
   integer                            :: iconst

!  allocate
   allocate(icelllist(Ndx))
   icelllist = 0

!  add a tracer if no current tracer is selected (in visualization)
   if ( ITRA1.eq.0 .or. iconst_cur.eq.0 .or. iconst_cur.lt.ITRA1 ) then ! note: tracers always at the back
      call add_tracer('', iconst)
!     set current tracer (for visualization)
      iconst_cur = iconst
   else
!     select current tracer
      iconst = iconst_cur
   end if

!  find active flow nodes
   Ncells = 0
   if ( NPL.le.2 ) then ! no (usable) polygon
      call in_flowcell(xp,yp,kk)
      if ( kk.gt.0 ) then
         Ncells = Ncells + 1
         icelllist(1) = kk
      end if
   else
      in = -1
      do kk=1,Ndx
         N = size(nd(kk)%x)
         call dbpinpol(xz(kk), yz(kk), in, dmiss, JINS, NPL, xpl, ypl, zpl)
         if ( in.eq.1 ) then
            Ncells = Ncells+1
            icelllist(Ncells) = kk
         end if
      end do
   end if

!  fill active flow nodes
   do i=1,Ncells
      kk = icelllist(i)
      do k=kbot(kk),kbot(kk) + kmxn(kk) - 1
         constituents(iconst,k) = constituents(iconst,k) + dval
      end do
   end do

!  plot
   call tekflowstuff(ja)

!  deallocate
   if ( allocated(icelllist) ) deallocate(icelllist)

   return
end subroutine droptracer
