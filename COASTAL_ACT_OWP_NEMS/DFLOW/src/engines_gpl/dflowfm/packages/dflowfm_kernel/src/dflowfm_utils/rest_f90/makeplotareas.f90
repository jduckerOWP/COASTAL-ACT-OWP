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

! $Id: makeplotareas.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/rest_f90/makeplotareas.f90 $

      SUBROUTINE MAKEPLOTAREAS(NUMROW,NUMCOL,nsize)
      implicit none
      double precision :: dx
      double precision :: dy
      integer :: i, nsize
      integer :: j
      integer :: nsc
      integer :: numcol
      integer :: numrow
      integer :: numsc
      double precision :: x1sc
      double precision :: x2sc
      double precision :: xb
      double precision :: xm
      double precision :: xz
      double precision :: y1sc
      double precision :: y2sc
      double precision :: yb
      double precision :: ym
      double precision :: yz
      COMMON /GSCREENS/ X1SC(100),Y1SC(100),X2SC(100),Y2SC(100),NUMSC
      NSC  = 0

      if (numrow == 1) then
         yz = 0.4d0 ; yb = 0.8d0*(1d0-yz)
      else
         yz = 0.7d0 ; yb = 0.8d0*(1d0-yz)
      endif

      if (numcol < 3) then
         xz = 0.7d0 ; xb = 0.5d0*(1d0-xz)
      else
         xz = 0.9d0 ; xb = 0.001d0 ! 05d0*(1d0-xz)
      endif

      if (nsize == 2) then
         yz = 0.45d0 ; yb = 0.8d0*(1d0-yz)
      endif
      DY = yz / NUMROW
      DX = xz / NUMCOL
      XM = DX / 40
      YM = DY / 40
      DO 10 J = 1,NUMROW
         DO 10 I = 1,NUMCOL
            NSC = NSC + 1
            X1SC(NSC) = xb + (I-1)*DX + XM
            X2SC(NSC) = xb + (I  )*DX - XM
            Y1SC(NSC) = 1d0-yb - (J  )*DY + YM
            Y2SC(NSC) = 1d0-yb - (J-1)*DY - YM
   10 CONTINUE
      NUMSC = NSC
      RETURN
      END
