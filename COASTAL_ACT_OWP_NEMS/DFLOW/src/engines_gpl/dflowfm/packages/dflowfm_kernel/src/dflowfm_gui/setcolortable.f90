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

! $Id: setcolortable.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/setcolortable.f90 $

      SUBROUTINE SETCOLORTABLE()
      implicit none
      double precision :: VMAX,VMIN,DV,VAL
      integer          :: NCOLS,NV,NIS,NIE,JAAUTO

      double precision :: VMAX2,VMIN2,DV2,VAL2
      integer          :: NCOLS2,NV2,NIS2,NIE2,JAAUTO2

      integer          :: i

      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
      COMMON /DEPMAX2/ VMAX2,VMIN2,DV2,VAL2(256),NCOLS2(256),NV2,NIS2,NIE2,JAAUTO2

      NIS  = 72
      DO I = 1,256
         NCOLS(I) = MIN(255,NIS + I-1)
      enddo

      NIS2 = 136
      DO I = 1,256
         NCOLS2(I) = MIN(255,NIS2 + I-1)
      enddo
      end SUBROUTINE SETCOLORTABLE
