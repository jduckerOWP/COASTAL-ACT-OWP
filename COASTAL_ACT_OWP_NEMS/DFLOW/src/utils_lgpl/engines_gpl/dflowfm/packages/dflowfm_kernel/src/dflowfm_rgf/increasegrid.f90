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

! $Id: increasegrid.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_rgf/increasegrid.f90 $

      SUBROUTINE INCREASEGRID(M,N)
      USE M_GRID
      USE M_MISSING
      use m_alloc
      implicit none
      integer :: m, n

      integer, dimension(2) :: ibounds, iboundsp1

      !if (m <= mmax .and. n <= nmax) return
      !Freshly allocate arrays, so that size fits exactly (e.g., for passing as 2D arrays to ecrrea)
!      if (allocated(xc)) deallocate (xc,yc,zc,ijc,ijyes)

!      mmax = m ; nmax = n ; MNMAX = MAX(M,N)
!      ibounds   = (/ mmax, nmax /)
!      iboundsp1 = (/ mmax+1, nmax+1 /)

      mmax = m+1 ; nmax = n+1 ; MNMAX = MAX(M,N)
      ibounds   = (/ mmax, nmax /)
      iboundsp1 = ibounds

      call realloc(xc, ibounds, fill=dxymis)
      call realloc(yc, ibounds, fill=dxymis)
      call realloc(zc, iboundsp1, fill=dxymis)
      call realloc(ijc, ibounds, fill=0)
      call realloc(ijyes, ibounds, fill=0)

      END SUBROUTINE INCREASEGRID
