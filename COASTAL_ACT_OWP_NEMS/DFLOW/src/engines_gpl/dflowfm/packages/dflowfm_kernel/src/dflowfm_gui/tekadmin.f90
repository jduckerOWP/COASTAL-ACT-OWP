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

! $Id: tekadmin.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/tekadmin.f90 $

      SUBROUTINE TEKADMIN(X,Y,I,J)
      implicit none
      integer :: i
      integer :: j
      integer :: l
      double precision :: x
      double precision :: y
      CHARACTER TEX*11
      IF (I .LE. 9) THEN
         WRITE(TEX(1:1) ,'(I1)') I
         L = 2
      ELSE IF (I .LE. 99) THEN
         WRITE(TEX(1:2) ,'(I2)') I
         L = 3
      ELSE IF (I .LE. 999) THEN
         WRITE(TEX(1:3) ,'(I3)') I
         L = 4
      ELSE
         WRITE(TEX(1:4) ,'(I4)') I
         L = 5
      ENDIF
      WRITE(TEX(L:L),'(A)') ','
      IF (J .LE. 9) THEN
         WRITE(TEX(L+1:L+1) ,'(I1)') J
         L = L + 1
      ELSE IF (J .LE. 99) THEN
         WRITE(TEX(L+1:L+2) ,'(I2)') J
         L = L + 2
      ELSE IF (J .LE. 999) THEN
         WRITE(TEX(L+1:L+3) ,'(I3)') J
         L = L + 3
      ELSE
         WRITE(TEX(L+1:L+4) ,'(I4)') J
         L = L + 4
      ENDIF
      CALL DRAWTEXT(real(X),real(Y), TEX(1:L))
      RETURN
      END
