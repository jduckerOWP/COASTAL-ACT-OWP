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

! $Id: poltolines.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/poltolines.f90 $

  SUBROUTINE POLTOLINES()

  use m_netw
  use m_afmeting
  use gridoperations

  implicit none
  double precision :: ael
  double precision :: ag
  double precision :: cfl
  double precision :: e0
  double precision :: eps
  integer :: k
  integer :: k1
  integer :: k2
  double precision :: pi
  double precision :: rho
  double precision :: rhow
  double precision :: rml
  double precision :: zp
  COMMON /CONSTANTS/ E0, RHO, RHOW, CFL, EPS, AG, PI
  DOUBLE PRECISION DLENGTH

  AEL    = PI*RDIAM*RDIAM/4                  ! RDIAM in mm
  DO K = 1,NPL-1
    CALL ISNODE( K1, XPL(K), YPL(K), ZP )
    IF (K1 .EQ. 0) THEN
       CALL GIVENEWNODENUM(K1)
       CALL SETPOINT(XPL(K),YPL(K),ZP,K1)
    ENDIF
    CALL ISNODE( K2, XPL(K+1), YPL(K+1), ZP )
    IF (K2 .EQ. 0) THEN
       CALL GIVENEWNODENUM(K2)
       CALL SETPOINT(XPL(K+1),YPL(K+1),ZP,K2)
    ENDIF
    RML = DLENGTH(K1,K2)
    CALL CONNECT(K1,K2,LFAC,AEL,RML)
  ENDDO
  RETURN
  END SUBROUTINE POLTOLINES
