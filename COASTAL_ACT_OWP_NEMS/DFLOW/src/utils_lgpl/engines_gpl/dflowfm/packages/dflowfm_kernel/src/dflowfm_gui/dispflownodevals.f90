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

! $Id: dispflownodevals.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/dispflownodevals.f90 $

SUBROUTINE DISPFLOWNODEVALS(KP)
  use m_flowgeom
  use m_flow
  USE M_DEVICES

  implicit none
  DOUBLE PRECISION :: ZNOD
  integer :: KP

  integer :: l
  integer :: n
  CHARACTER TEX*23

  IF (KP .EQ. 0) RETURN
  CALL DRCIRC(XZ(KP),YZ(KP),BL(KP))

  TEX = 'NODE NR    :           '
  WRITE(TEX (14:),'(I10)') KP
  CALL KTEXT(TEX,IWS-22,4,15)

  TEX = 'X COORD    :           '
  WRITE(TEX (14:),'(E10.3)') Xz(KP)
  CALL KTEXT(TEX,IWS-22,13,15)

  TEX = 'Y COORD    :           '
  WRITE(TEX (14:),'(E10.3)') Yz(KP)
  CALL KTEXT(TEX,IWS-22,14,15)

  TEX = 'Z COORD    :           '
  WRITE(TEX (14:),'(E10.3)') bl(KP)
  CALL KTEXT(TEX,IWS-22,15,15)

  TEX = 'Z COORD    :           '
  WRITE(TEX (14:),'(e10.4)') znod(kp)
  CALL KTEXT(TEX,IWS-22,16,15)

  TEX = 'link       :           '
  DO N = 1,Nd(kp)%lnx
     L = ND(KP)%LN(N)
     WRITE(TEX ( 6:11),'(I6 )') N
     WRITE(TEX (14:23),'(I10)') L
     CALL KTEXT(TEX,IWS-22,16+N,15)
  ENDDO
  end SUBROUTINE DISPFLOWNODEVALS
