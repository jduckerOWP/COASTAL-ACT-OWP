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

! $Id: tridag.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/tridag.f90 $

! =================================================================================================
! =================================================================================================
 subroutine tridag_dflow(a,b,c,d,e,u,n)
 implicit none
 integer          :: n, j
 double precision :: a(n),b(n),c(n),d(n),e(n),u(n), bet, accur = 1d-15

 bet =b(1)
 u(1)=d(1)/bet
 do j=2,n
    e(j)=c(j-1)/bet
    bet=b(j)-a(j)*e(j)
    if (abs(bet) < accur) then
        bet = sign(accur,bet)
    endif
    u(j)=(d(j)-a(j)*u(j-1))/bet
 enddo

 do j=n-1,1,-1
    u(j)=u(j)-e(j+1)*u(j+1)
 enddo
 end subroutine tridag_dflow
