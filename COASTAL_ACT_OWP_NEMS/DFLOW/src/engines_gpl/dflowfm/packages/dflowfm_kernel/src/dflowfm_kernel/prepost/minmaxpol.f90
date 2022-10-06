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

! $Id: minmaxpol.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/minmaxpol.f90 $

   SUBROUTINE MINMAXPOL(XMIN, YMIN, XMAX, YMAX)
   USE M_POLYGON
   USE M_MISSING
   implicit none
   double precision :: XMIN, YMIN, XMAX, YMAX

   integer :: k
   XMAX = -1E30; XMIN = -XMAX
   YMAX = -1E30; YMIN = -YMAX
   DO K = 1,NPL
      IF (XPL(K) .NE. XYMIS) THEN
         XMAX = MAX(XPL(K),XMAX)
         YMAX = MAX(YPL(K),YMAX)
         XMIN = MIN(XPL(K),XMIN)
         YMIN = MIN(YPL(K),YMIN)
      ENDIF
   ENDDO
   END SUBROUTINE MINMAXPOL
