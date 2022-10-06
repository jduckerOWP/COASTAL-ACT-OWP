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

! $Id: spldist.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_rgf/spldist.f90 $

      DOUBLE PRECISION FUNCTION SPLDIST(X,X2,Y,Y2,XX,YY,TV,N)
      implicit none
      integer :: n
!     AFSTAND VAN PUNT XX,YY TOT SPLINEPUNT MET PARM TV

      DOUBLE PRECISION :: X(N), X2(N), Y(N), Y2(N), TV, XX, YY, XV, YV
      TV = MAX(0d0,MIN(TV,N-1d0))
      CALL SPLINT(X,X2,N,TV,XV)
      CALL SPLINT(Y,Y2,N,TV,YV)
!     CALL DISTANCE(XV,YV,XX,YY,DIST)
      CALL PLANEDISTANCE(XV,YV,XX,YY,SPLDIST)
      RETURN
      END FUNCTION SPLDIST
