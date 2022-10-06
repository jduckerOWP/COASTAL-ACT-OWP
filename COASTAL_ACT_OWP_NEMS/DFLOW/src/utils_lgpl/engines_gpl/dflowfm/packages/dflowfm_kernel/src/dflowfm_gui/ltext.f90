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

! $Id: ltext.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/ltext.f90 $

      SUBROUTINE LTEXT(TEX,NX,NY,NCOL)
      use unstruc_colors
      implicit none
      integer :: ncol
      integer :: ndraw
      integer :: nx
      integer :: ny
      double precision :: x
      double precision :: y
!     grafische tekst op normale text posities
      CHARACTER TEX*(*)
      COMMON /DRAWTHIS/ ndraw(50)
      X = X1 + (X2-X1)*dble(NX)/dble(IWS)
      Y = Y2 + (Y1-Y2)*dble(NY)/dble(IHS)
      IF (NDRAW(10) .EQ. 1) THEN
         CALL SETCOL(1)
      ELSE
         CALL SETCOL(KLTEX)
      ENDIF
      CALL DRAWTEXT(real(X),real(Y),TEX)
      RETURN
      END
