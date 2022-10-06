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

! $Id: copysamtopol.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/copysamtopol.f90 $

!> copy samples to polygon (for further operations)
subroutine copysamtopol()

   use M_SAMPLES
   use m_missing, only: dmiss, JINS
   use m_polygon, only: NPL, xpl, ypl, zpl, increasepol, savepol
   use geometry_module, only: dbpinpol

   implicit none

   integer, dimension(:), allocatable :: jacopy ! sample wil be copied (1) or not (0)

   integer                            :: i, inside, numcopy

!  allocate
   allocate(jacopy(Ns))
   jacopy = 1
   numcopy = NS

!  check if selecting polygon exists
   if ( NPL.gt.2 ) then
!     mark and count samples to be copied to polygon
      inside = -1 ! initialization of dbpinpol
      do i=1,NS
         call dbpinpol(xs(i), ys(i), inside, dmiss, JINS, NPL, xpl, ypl, zpl)
         if ( inside.ne.1 ) then
            jacopy(i) = 0
            numcopy = numcopy-1
         end if
      end do
   end if

!  check if samples were selected
   if ( numcopy.gt.0 ) then
!     copy selected samples to polygon
      call savepol()

      call increasepol(numcopy,0)

      NPL = 0
      do i=1,NS
         if ( jacopy(i).eq.1 ) then
            NPL=NPL+1
            xpl(NPL) = xs(i)
            ypl(NPL) = ys(i)
            zpl(NPL) = zs(i)
         end if
      end do
   end if

!  deallocate
   if ( allocated(jacopy) ) deallocate(jacopy)

   return
end subroutine copysamtopol
