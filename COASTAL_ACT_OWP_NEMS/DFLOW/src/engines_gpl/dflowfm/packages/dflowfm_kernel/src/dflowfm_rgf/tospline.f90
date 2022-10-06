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

! $Id: tospline.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_rgf/tospline.f90 $

       SUBROUTINE TOSPLINE(XX, YY, XV, YV)
       USE M_SPLINES
       implicit none

       double precision :: XX, YY, XV, YV

       double precision :: XI(maxsplen), XI2(maxsplen), YI(maxsplen), YI2(maxsplen)
       double precision :: TV, DIS
       integer :: IN, NUMPI

       IN = 1 ! Pick first spline
       CALL NUMP(IN, NUMPI)
       TV = NUMPI/2d0
       CALL GETIJ (XSP, XI, maxspl, maxsplen, maxsplen, IN, IN,  1, NUMPI)
       CALL GETIJ (YSP, YI, maxspl, maxsplen, maxsplen, IN, IN,  1, NUMPI)
       CALL SPLINE(XI,NUMPI,XI2)
       CALL SPLINE(YI,NUMPI,YI2)
       CALL DISMIN(XI,XI2,YI,YI2,XX,YY,NUMPI, DIS,TV,XV,YV)
       RETURN
       END subroutine tospline
