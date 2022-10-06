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

! $Id: tekpreviousnet.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/tekpreviousnet.f90 $

      SUBROUTINE TEKPREVIOUSNET(NCOL)
      use m_netw
      implicit none
      integer :: NCOL

      integer :: k
      integer :: k1
      integer :: k2
      integer :: l
      integer :: ndraw

      COMMON /DRAWTHIS/ ndraw(50)
      IF (NDRAW(16) .LE. 0) RETURN
      CALL SETCOL(NCOL)
      DO L = 1,NUML0
         K1 = KN0(1,L)
         K2 = KN0(2,L)
         IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
            CALL DMOVABS( XK0(K1),YK0(K1),ZK0(K1) )
            CALL  DLNABS( XK0(K2),YK0(K2),ZK0(K2) )
         ENDIF
      ENDDO
      END SUBROUTINE TEKPREVIOUSNET
