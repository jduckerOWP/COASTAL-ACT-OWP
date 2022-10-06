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

! $Id: unstruc_ini.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/unstruc_ini.f90 $

module unstruc_ini
!! Some basic routines for reading an INI file.
!! Most work is done in startup and model modules.

use unstruc_messages
use properties

implicit none
private ! Prevent used modules from being exported

   type(tree_data), pointer, public :: ini_ptr !< Unstruc ini settings in tree_data

public :: readIniFile, &
          get_req_string, get_req_integer, get_req_integers, get_req_double

contains

!> Loads initial program settings file through inifiles. 
subroutine readIniFile(filename, istat)
    character(*),      intent(in)  :: filename
    integer, optional, intent(out) :: istat

    istat = 0 ! Success

    call tree_create(trim(filename), ini_ptr)
    call prop_file('ini',trim(filename),ini_ptr,istat)
    if (istat /= 0) then
        call mess(LEVEL_ERROR, 'ini file '''//trim(filename)//''' not found. Code: ', istat)
    endif
end subroutine readIniFile

!> Reads the value for a string variable from a propery tree.
!! When not found, and error is logged and program stops. 
subroutine get_req_string(prop_ptr, chapter, key, value)
    type(tree_data), pointer, intent(in) :: prop_ptr
    character(*) ,intent (in)  :: chapter
    character(*) ,intent (in)  :: key
    character(*) ,intent (out) :: value

    logical :: success

    call prop_get_string(prop_ptr, chapter, key, value, success)
    if (.not. success) then
      call err('ERROR READING INI-FILE, RESTORE CORRECT FILE OR CALL Deltares.',&
               'NO VALUE FOUND FOR: ',key)
    endif
end subroutine get_req_string

!> Reads the value for an integer variable from a propery tree.
!! When not found, and error is logged and program stops. 
subroutine get_req_integer(prop_ptr, chapter, key, value)
    type(tree_data), pointer, intent(in) :: prop_ptr
    character(*) ,intent (in)  :: chapter
    character(*) ,intent (in)  :: key
    integer      ,intent (out) :: value

    logical :: success

    call prop_get_integer(prop_ptr, chapter, key, value, success)
    if (.not. success) then
      call err('ERROR READING INI-FILE, RESTORE CORRECT FILE OR CALL Deltares.',&
               'NO VALUE FOUND FOR: ',key)
    endif
end subroutine get_req_integer


!> Reads the value for an integer-list variable from a propery tree.
!! When not found, and error is logged and program stops. 
subroutine get_req_integers(prop_ptr, chapter, key, value, valuelength)
    type(tree_data), pointer, intent(in) :: prop_ptr
    character(*) ,intent (in)  :: chapter
    character(*) ,intent (in)  :: key
    integer,dimension(*),intent (out) :: value
    integer, intent (in) :: valuelength

    logical :: success

    call prop_get_integers(prop_ptr, chapter, key, value, valuelength, success)
    if (.not. success) then
      call err('ERROR READING INI-FILE, RESTORE CORRECT FILE OR CALL Deltares.\n'//&
               'NO VALUE FOUND FOR: '//key)
    endif
end subroutine get_req_integers

!> Reads the value for a double precision variable from a propery tree.
!! When not found, and error is logged and program stops. 
subroutine get_req_double(prop_ptr, chapter, key, value)
    type(tree_data), pointer, intent(in) :: prop_ptr
    character(*)    ,intent (in)  :: chapter
    character(*)    ,intent (in)  :: key
    double precision,intent (out) :: value

    logical :: success

    call prop_get_double(prop_ptr, chapter, key, value, success)
    if (.not. success) then
      call err('ERROR READING INI-FILE, RESTORE CORRECT FILE OR CALL Deltares.',&
               'NO VALUE FOUND FOR: ',key)
    endif
end subroutine get_req_double



end module unstruc_ini
