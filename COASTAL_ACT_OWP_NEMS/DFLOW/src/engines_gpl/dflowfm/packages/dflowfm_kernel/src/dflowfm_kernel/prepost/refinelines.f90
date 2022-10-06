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

! $Id: refinelines.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/refinelines.f90 $

  SUBROUTINE REFINELINES()
  use m_netw
  USE M_GRIDSETTINGS
  use m_missing, only: dmiss, jins
  use geometry_module, only: pinpok
  implicit none

  integer :: INL
  integer :: k1
  integer :: k2
  integer :: l
  integer :: lnu
  double precision :: a0, r0, XX, YY, ZZ

  IF (MFAC .LE. 1) RETURN

  DO L  = 1,NUML
     K1 = KN(1,L)
     K2 = KN(2,L)
     XX = 0.5D0*( XK(K1) + XK(K2) )
     YY = 0.5D0*( YK(K1) + YK(K2) )
     ZZ = 0.5D0*( ZK(K1) + ZK(K2) )

     CALL PINPOK( XX, YY, NPL, XPL, YPL, INL, jins, dmiss)
     IF (INL .EQ. 1) THEN

        CALL DELELEM(K1,K2,LNU)
        CALL CONNECT(K1,K2,mFAC,A0, R0)

     ENDIF

  ENDDO

  RETURN
  END SUBROUTINE REFINELINES