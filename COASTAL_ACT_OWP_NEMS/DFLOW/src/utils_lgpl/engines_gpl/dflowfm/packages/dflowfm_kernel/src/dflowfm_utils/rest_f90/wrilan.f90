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

! $Id: wrilan.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/rest_f90/wrilan.f90 $

      SUBROUTINE WRILAN(MPOL)
      USE M_LANDBOUNDARY
      implicit none
      integer :: mpol
      integer :: mx
      double precision, ALLOCATABLE :: XL(:), YL(:)
      double precision :: ZL(0)    ! no z-values
      character(len=1) :: names(1) ! no names

      MX = MAXLAN
      ALLOCATE ( XL(MX), YL(MX))
      XL (1:MXLAN)  = XLAN(1:MXLAN)
      YL (1:MXLAN)  = YLAN(1:MXLAN)
      names = ' '

      CALL WRILDB(MPOL, XL, YL, MXLAN, nclan, MXLAN, ZL, 0, names, 1, 1)
      DEALLOCATE (XL, YL)

      END
