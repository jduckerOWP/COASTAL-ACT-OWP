subroutine nc_check_err(lundia, ierror, description, filename)
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
!  $Id: nc_check_err.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/flow2d3d/packages/data/src/general/nc_check_err.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use netcdf
    !
    implicit none
!
! Global variables
!
    integer     , intent(in) :: lundia  !  Description and declaration in inout.igs
    integer     , intent(in) :: ierror
    character(*), intent(in) :: description
    character(*), intent(in) :: filename
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    if (ierror /= nf90_noerr) then
        write (*,'(6a)') 'ERROR ', trim(description), '. NetCDF file : "', trim(filename), '". Error message:', nf90_strerror(ierror)
        write (lundia,'(6a)') 'ERROR ', trim(description), '. NetCDF file : "', trim(filename), '". Error message:', nf90_strerror(ierror)
    endif
end subroutine nc_check_err
