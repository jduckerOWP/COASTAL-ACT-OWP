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

! $Id: writesluices.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_io/writesluices.f90 $

 subroutine writesluices()
 integer          :: mgat = 0, mgat2 = 0, k, j
 double precision :: t(6), h(6), tim, zgt, bed

 if (mgat .ne. 0) return

 t(1) =  0d0 ; h(1) =  0.0d0   ! door closed                     ! sea side
 t(2) =  2d0 ; h(2) =  0.2d0   ! open the gates
 t(3) =  3d0 ; h(3) =  0.2d0   ! levelling through open gates
 t(4) =  2d0 ; h(4) = 12.0d0   ! open the door
 t(5) = 12d0 ; h(5) = 12.0d0   ! boats through open door
 t(6) =  2d0 ; h(6) =  0.0d0   ! close the door

 tim = 0d0
 call newfil(mgat,  'zeegate_0001.tom')
 call newfil(mgat2, 'landgate_0001.tom')

 tim = 0d0 ; bed = -11d0
 write(mgat2,*) tim, bed

 do k = 1,100
    do j = 1,6
       tim = tim + t(j)
       zgt = bed + h(j)
       write(mgat,*) tim, zgt
    enddo
    do j = 1,6
       tim = tim + t(j)
       zgt = bed + h(j)
       write(mgat2,*) tim, zgt
    enddo

 enddo
 call doclose (mgat)  ; mgat  = -1
 call doclose (mgat2) ; mgat2 = -1

 end subroutine writesluices
