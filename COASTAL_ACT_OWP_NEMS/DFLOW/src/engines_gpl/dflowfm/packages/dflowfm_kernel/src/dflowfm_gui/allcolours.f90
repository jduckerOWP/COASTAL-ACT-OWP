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

! $Id: allcolours.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/allcolours.f90 $

      SUBROUTINE ALLCOLOURS()
      use m_wearelt
      implicit none
      double precision :: dx
      double precision :: dxc
      double precision :: dy
      double precision :: dyc
      integer :: i
      integer :: j
      integer :: ncol
      double precision :: x
      double precision :: xc
      double precision :: xl
      double precision :: xu
      double precision :: y
      double precision :: yc
      double precision :: yl
      double precision :: yu
      NCOL = 0
      XL   = X2-0.66d0*DSIX-RCIR*4
      XU   = XL+0.66d0*DSIX
      YL   = Y1+DSIX
      YU   = Y2-DSIX
      DX   = XU-XL
      DY   = YU-YL
      DXC  = DX/20
      DYC  = DY/20
      DO 10 J = 1,16
         DO 10 I = 1,16
            X  = dble(I-1)/15d0
            Y  = dble(J-1)/15d0
            XC = XL + X*DX
            YC = YL + Y*DY
            CALL SETCOL(NCOL)
            NCOL = NCOL + 1
            CALL FBOXnop(XC-DXC,YC-DYC,XC+DXC,YC+DYC)
            CALL SETCOL(0)
            CALL  BOXnop(XC-DXC,YC-DYC,XC+DXC,YC+DYC)
   10 CONTINUE
      RETURN
      END
