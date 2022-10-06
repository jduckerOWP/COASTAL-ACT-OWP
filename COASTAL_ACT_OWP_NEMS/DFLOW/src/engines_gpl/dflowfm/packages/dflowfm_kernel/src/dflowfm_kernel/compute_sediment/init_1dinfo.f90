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

! $Id: init_1dinfo.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_sediment/init_1dinfo.f90 $

   subroutine init_1dinfo()
   use m_flowgeom, only: lnx1D, ln, ndx
   use m_sediment, only: stmpar
   use m_fm_erosed, only: link1, link1sign, link1_initialized

   integer :: k1
   integer :: k2
   integer :: L

   if (.not. stmpar%morpar%mornum%pure1d) return
   if (link1_initialized) return

   ! if (isassociated(link1)) deallocate(link1, link1sign) ! if link1 were associated then link1_initailized is true and this statement isn't reached
   allocate(link1(ndx), link1sign(ndx))

   ! we define the node as the begin/end point of the first link connected to it
   link1(:) = 0
   link1sign(:) = 0
   do L = 1,lnx1D
       k1 = ln(1,L)
       k2 = ln(2,L)
       if (link1(k1) == 0) then
           link1(k1) = L
           link1sign(k1) = -1
       endif
       if (link1(k2) == 0) then
           link1(k2) = L
           link1sign(k2) = 1
       endif
   enddo
   link1_initialized = .true.
   end subroutine init_1dinfo
