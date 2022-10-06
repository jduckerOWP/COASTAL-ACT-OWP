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

! $Id: wricrs.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/rest_f90/wricrs.f90 $

      !> Writes active cross sections to a polyline file.
      subroutine wricrs(mpol)
      use m_crosssections
      use m_polygon
      use m_missing
      implicit none
      integer :: mpol,i

      call savepol()
      call copycrosssectionstopol()
!      npl = 0 ! Write traced polygons instead of original plis
!      do i=1,ncrs
!        xpl(npl+1:npl+crs(i)%len+1)=crs(i)%xk(1:crs(i)%len+1)
!        ypl(npl+1:npl+crs(i)%len+1)=crs(i)%yk(1:crs(i)%len+1)
!        npl = npl+crs(i)%len+2
!        xpl(npl) = dmiss
!        ypl(npl) = dmiss
!      end do
!      if (ncrs>0) npl = npl - 1 ! remove last separator
      call wripol(mpol)
      call restorepol()

      end subroutine wricrs
