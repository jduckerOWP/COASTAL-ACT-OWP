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

! $Id: removecoincidingtriangles.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/removecoincidingtriangles.f90 $

   SUBROUTINE REMOVECOINCIDINGTRIANGLES()
   use m_netw                     ! 2 REMOVES SMALL TRIANGLES NEXT TO
   USE M_FLOWGEOM
   use unstruc_messages
   use m_sferic
   use geometry_module, only: getdxdy
   use gridoperations
   implicit none

   DOUBLE PRECISION :: DX2,DY2,DX3,DY3,DEN
   INTEGER          :: K1, K2, K3, KDUM, N, L, LL, JA, IERR

   DOUBLE PRECISION, ALLOCATABLE :: XNW(:), YNW(:)
   INTEGER         , ALLOCATABLE :: NNW(:,:)

   DOUBLE PRECISION, EXTERNAL    :: getdx, getdy

   CALL FINDCELLS(3)

   ALLOCATE ( XNW(NUMP),YNW(NUMP),NNW(3,NUMP) , STAT=IERR   )
   CALL AERR('XNW(NUMP),YNW(NUMP),NNW(3,NUMP)', IERR, NUMK*3)
   NNW = 0
   DO N   = 1, NUMP ! REMOVE COINCIDING TRIANGLES
      K1  = NETCELL(N)%NOD(1); K2 = NETCELL(N)%NOD(2) ; K3 = NETCELL(N)%NOD(3)

!     fix for spherical, periodic coordinates
      if ( jsferic.eq.1 .and. abs(abs(yk(k1))-90d0).lt.dtol_pole ) then
         kdum = k1
         k1   = k2
         k2   = k3
         k3   = kdum
      end if

      !dx2 = getdx(XK(K1), YK(K1), XK(K2), YK(K2)) ! AvD: TODO: getdx toepassen
      !dy2 = getdy(XK(K1), YK(K1), XK(K2), YK(K2))
      call getdxdy(XK(K1), YK(K1), XK(K2), YK(K2), dx2,dy2, jsferic)
      !dx3 = getdx(XK(K1), YK(K1), XK(K3), YK(K3))
      !dy3 = getdy(XK(K1), YK(K1), XK(K3), YK(K3))
      call getdxdy(XK(K1), YK(K1), XK(K3), YK(K3), dx3,dy3, jsferic)
      den = dy2*dx3-dy3*dx2
      IF (DEN == 0D0) THEN
         DO LL = 1,3
            L  = NETCELL(N)%LIN(LL)
            KN(1,L) = 0 ; KN(2,L) = 0
         ENDDO
         NNW(1,N) = K1 ;NNW(2,N) = K2 ; NNW(3,N) = K3
         XNW(N)   = (XK(K1) + XK(K2) + XK(K3)) / 3D0
         YNW(N)   = (YK(K1) + YK(K2) + YK(K3)) / 3D0
      ENDIF
   ENDDO

   DO N  = 1,NUMP
      K1 = NNW(1,N) ; K2 = NNW(2,N) ; K3 = NNW(3,N)
      IF (K1 > 0) THEN
         XK(K1) = XNW(N)
         YK(K1) = YNW(N)
         CALL MERGENODES(K2,K1,JA)
         CALL MERGENODES(K3,K1,JA)
      ENDIF
   ENDDO

   DEALLOCATE(XNW,YNW,NNW)
   END SUBROUTINE REMOVECOINCIDINGTRIANGLES
