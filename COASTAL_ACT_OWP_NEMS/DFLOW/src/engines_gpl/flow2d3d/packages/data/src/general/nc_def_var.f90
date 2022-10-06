function nc_def_var(idfile, lundia, name, datatype, ndims, dims, standardname, longname, unit, xycoordinates, filename) result(idvar)
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
!  $Id: nc_def_var.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/flow2d3d/packages/data/src/general/nc_def_var.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use netcdf
    !
    implicit none
    !
    ! return value
    integer :: idvar
!
! Global variables
!
    integer                  , intent(in) :: idfile
    integer                  , intent(in) :: lundia  !  Description and declaration in inout.igs
    character(*)             , intent(in) :: name
    integer                  , intent(in) :: datatype
    integer                  , intent(in) :: ndims
    integer, dimension(ndims), intent(in) :: dims
    character(*)             , intent(in) :: standardname
    character(*)             , intent(in) :: longname
    character(*)             , intent(in) :: unit
    logical                  , intent(in) :: xycoordinates
    character(*)             , intent(in) :: filename
!
! Local variables
!
    integer :: ierror
    integer :: returnvalue
!
!! executable statements -------------------------------------------------------
!
    ierror = nf90_def_var(idfile, name, datatype, dims, returnvalue)
    call nc_check_err(lundia, ierror, "def_var "//name, trim(filename))
    if (xycoordinates) then
       ierror = nf90_put_att(idfile, returnvalue,  'coordinates'  , 'x y'); call nc_check_err(lundia, ierror, "put_att "//name//" coordinates", trim(filename))
    endif
    ierror = nf90_put_att(idfile, returnvalue,  'standard_name', standardname); call nc_check_err(lundia, ierror, "put_att "//name//" standard_name", trim(filename))
    ierror = nf90_put_att(idfile, returnvalue,  'long_name'    , longname); call nc_check_err(lundia, ierror, "put_att "//name//" longname", trim(filename))
    ierror = nf90_put_att(idfile, returnvalue,  'units'        , unit); call nc_check_err(lundia, ierror, "put_att "//name//" units", trim(filename))
    idvar = returnvalue
end function nc_def_var
