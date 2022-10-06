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

! $Id: ispumpon.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/ispumpon.f90 $

 integer function ispumpon(n,s1k)
 use m_flowexternalforcings
 use m_missing
 use m_structures

 integer, intent(in) :: n
 double precision, intent(in) :: s1k
 ! this is for safety, check arrays before dereference
 if (.not.allocated(pumponoff)) then
     ispumpon = 1
     return
 endif

 if (pumponoff(1,n) == dmiss .and. pumponoff(2,n) == dmiss) then
    ispumpon = 1 ; return
 endif
 if (pumponoff(1,n) .ne. dmiss .and. s1k > pumponoff(1,n) ) then
     pumponoff(5,n) = 1
 endif
 if (pumponoff(2,n) .ne. dmiss .and. s1k < pumponoff(2,n) ) then
     pumponoff(5,n) = 0
 endif
 ispumpon = int(pumponoff(5,n)) ! no change
end function ispumpon
