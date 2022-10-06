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

! $Id: shifxy.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_rgf/shifxy.f90 $

      !> Operates on active grid from m_grid directly!
      SUBROUTINE SHIFXY(IS,     JS,     MP,     NP        )

      !     XH,     YH,     mmax, nmax, MC,     NC, IS,     JS,     MP,     NP        )
      use m_missing
      use m_grid
      use geometry_module, only: pinpok

      implicit none
      integer :: is, js, mp, np

      integer :: i, j
!     schuif data naar rechts of boven of beide en geef nieuwe MC,NC

      MC = MC + IS
      NC = NC + JS

      call increasegrid(mc,nc)

      MP = MP + IS
      NP = NP + JS

      DO 10 J = NC,1+JS,-1
         DO 10 I = MC,1+IS,-1
            Xc(I,J) = Xc(I-IS,J-JS)
            Yc(I,J) = Yc(I-IS,J-JS)
            Zc(I,J) = Zc(I-IS,J-JS)
    10 CONTINUE
      IF (IS .EQ. 1) THEN
         DO 20 J = 1,NC
            Xc(1,J) = XYMIS
            Yc(1,J) = XYMIS
            Zc(1,J) = XYMIS
    20   CONTINUE
      ENDIF
      IF (JS .EQ. 1) THEN
         DO 30 I = 1,MC
            Xc(I,1) = XYMIS
            Yc(I,1) = XYMIS
            Zc(I,1) = XYMIS
    30   CONTINUE
      ENDIF
      RETURN
      END subroutine shifxy
