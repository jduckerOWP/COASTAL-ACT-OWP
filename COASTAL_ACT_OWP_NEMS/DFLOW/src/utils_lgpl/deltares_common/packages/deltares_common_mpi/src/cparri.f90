subroutine cparri ( iarr1, iarr2, length )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2022.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: cparri.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/utils_lgpl/deltares_common/packages/deltares_common_mpi/src/cparri.f90 $
!!--description-----------------------------------------------------------------
!
!   Copies integer array IARR1 to IARR2
!
!!--pseudo code and references--------------------------------------------------
!
!
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer,                      intent(in)  :: length ! array length
    integer, dimension(1:length), intent(in)  :: iarr1  ! source array
    integer, dimension(1:length), intent(out) :: iarr2  ! target array
!
! Local variables
!
    integer :: i ! loop counter
!
!! executable statements -------------------------------------------------------
!
    do i = 1, length
       iarr2(i) = iarr1(i)
    enddo
end subroutine cparri
