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

! $Id: getwavenrqn.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_waves/getwavenrqn.f90 $

SUBROUTINE getwavenrqn(DEPTH,Period, RK)
use m_sferic
implicit none
double precision :: PERIOD,DEPTH,RK
double precision :: OMEGAS, GR, RLAB0, DEP2PI,  RLAB1, rlab2, criter
OMEGAS = TWOPI / PERIOD
GR     = 9.81
RLAB0  = TWOPI*GR / OMEGAS**2
DEP2PI = TWOPI*DEPTH
RLAB1  = RLAB0*SQRT( TANH(DEP2PI / RLAB0) )

10 CONTINUE
   RLAB2  = RLAB0*TANH(DEP2PI/RLAB1)
   CRITER = (RLAB2 - RLAB1) / RLAB1
   IF (ABS(CRITER) .LT. 0.001) THEN
      RK = TWOPI/RLAB2
      RETURN
   ELSE
      RLAB1 = RLAB2
      GOTO 10
   ENDIF

end SUBROUTINE getwavenrqn
