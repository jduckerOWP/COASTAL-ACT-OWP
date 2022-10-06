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
!  $Id: util.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/tools_gpl/datsel/packages/datsel_f/src/util.f90 $
!-------------------------------------------------------------------------------
!   Delft3D - Stubs for legacy C routines
!
!   Irv.Elshoff@deltares.nl
!   Adri.Mourits@deltares.nl
!   07 jun 05
!
!-------------------------------------------------------------------------------


subroutine cgetcp (cpu)
    double precision, intent (out) :: cpu

    call cutil_cgetcp (cpu)
end


subroutine cdate (date)
    character(len=*), intent (out) :: date

    call cutil_cdate (date)
end


subroutine cstop (exit_code, message)
    integer         , intent (in) :: exit_code
    character(len=*), intent (in) :: message

    call cutil_cstop (exit_code, message)
end

!
! Use the name "util_getenv" instead of "getenv".
! The last one conflicts with standard libraries on Windows
!
subroutine util_getenv (name, value)
    character(*), intent (in)  :: name
    character(*), intent (out) :: value

    call cutil_getenv (name, len(name), value, len(value))
end


subroutine getmp (error, pathd)
    logical,        intent(out)    :: error
    character(*),   intent(out)    :: pathd

    ! Routine to determine the location of the "default" directory.

    call cutil_getmp (pathd, len (pathd), result)

    if (result == 0) then
        error = .false.
    else
        error = .true.
    endif
end

!
! Use the name "util_system" instead of "system".
! The last one conflicts with standard libraries on Windows
!
subroutine util_system (command)
    character(*), intent (in) :: command

    call cutil_system (command, len(command))
end

