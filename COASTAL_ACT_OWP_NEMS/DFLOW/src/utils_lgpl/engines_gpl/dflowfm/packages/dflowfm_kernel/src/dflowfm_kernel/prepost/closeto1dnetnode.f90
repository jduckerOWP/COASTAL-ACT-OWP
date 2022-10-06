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

! $Id: closeto1dnetnode.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/closeto1dnetnode.f90 $

   SUBROUTINE CLOSETO1Dnetnode(XP1,YP1,N1,dist) !

      use m_netw
      use geometry_module, only: dbdistance
      use m_sferic
      use m_missing

      implicit none
      double precision, intent(in)  :: XP1, YP1
      double precision, intent(out) :: dist     ! find 1D point close to x,y:
      integer         , intent(out) :: n1       ! 1D point found


      double precision :: dismin
      integer          :: ja, k, k1, k2, L
      double precision :: dis,dis1,dis2

      N1 = 0
      DISMIN = 9E+33
      DO L = 1,numl
         IF (kn(3,L) == 1 .or. kn(3,L) == 6) then !  .or. kn(3,L) == 4) THEN
             K1 = kn(1,L) ; K2 = kn(2,L)
             dis1 = dbdistance(XP1,YP1,Xk(K1),Yk(K1),jsferic, jasfer3D, dmiss)
             dis2 = dbdistance(XP1,YP1,Xk(K2),Yk(K2),jsferic, jasfer3D, dmiss)
             if (dis1 < dis2) THEN
                k = k1 ; dis = dis1
             else
                k = k2 ; dis = dis2
             endif
             IF (DIS .LT. DISMIN) THEN
                N1 = k
                DISMIN = DIS
             ENDIF
          ENDIF
      ENDDO
      dist = dismin
      END SUBROUTINE CLOSETO1Dnetnode
