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

! $Id: teklink.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/teklink.f90 $

  SUBROUTINE TEKLINK(L,NCOL)
  use m_netw
  use unstruc_colors
  implicit none
  integer :: L, NCOL
  integer :: jaSmallCir
  double precision :: dlength

  integer :: k1
  integer :: k2

  CALL SETLINKCOLOUR(L,NCOL)

  K1 = KN(1,L)
  K2 = KN(2,L)
  IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
     CALL MOVABS( XK(K1),YK(K1) )
     CALL  LNABS( XK(K2),YK(K2) )
     IF (NCOL > 0) THEN
         CALL SETCOL(NCOLNN)
         CALL PTABS(XK(K1),YK(K1) )
         CALL PTABS(XK(K2),YK(K2) )
     ENDIF
  ENDIF
  RETURN
  END SUBROUTINE TEKLINK
