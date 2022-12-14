module agrhyd_version_module

!!  Copyright (C)  Stichting Deltares, 2012-2021.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

!  $Id: agrhyd_version.F90.svn 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL:  $
    use iso_c_binding

    implicit none

    character(*),  public, parameter :: agrhyd_major        = '**'
    character(*),  public, parameter :: agrhyd_minor        = '**'
    character(*),  public, parameter :: agrhyd_revision     = '**'
    character(*),  public, parameter :: agrhyd_build_number = '141013'

    character(*),  public, parameter :: agrhyd_company      = 'Deltares'
    character(*),  public, parameter :: agrhyd_company_url  = 'http://www.deltares.nl'
    character(*),  public, parameter :: agrhyd_program      = 'AGRHYD'
    character(*),  public, parameter :: agrhyd_programname  = 'AGRHYD'  ! use in about box and window title

    character(*),  public, parameter :: agrhyd_version      = agrhyd_major//'.'//agrhyd_minor//'.'//agrhyd_revision//'.'//agrhyd_build_number
    character(*),  public, parameter :: agrhyd_version_full = 'Deltares, '//agrhyd_program//' Version '//agrhyd_version//', '//__DATE__//', '//__TIME__
    character(*),  public, parameter :: agrhyd_version_id   = '@(#)'//agrhyd_version_full

contains

    subroutine getfullversionstring_agrhyd(stringout)
        character(*), intent(out) :: stringout
        integer                   :: length

        length = min(len_trim(agrhyd_version_full),len(stringout))
        stringout = agrhyd_version_id(5:5+length-1)
    end subroutine getfullversionstring_agrhyd

    subroutine getprogramnamestring_agrhyd(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(agrhyd_programname)
    end subroutine getprogramnamestring_agrhyd

    subroutine getshortprogramnamestring_agrhyd(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(agrhyd_program)
    end subroutine getshortprogramnamestring_agrhyd

    subroutine getfeaturenumberstring_agrhyd(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(agrhyd_version)
    end subroutine getfeaturenumberstring_agrhyd

    subroutine getversionnumberstring_agrhyd(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(agrhyd_version)
    end subroutine getversionnumberstring_agrhyd

    subroutine getcompanystring_agrhyd(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(agrhyd_company)
    end subroutine getcompanystring_agrhyd

    subroutine getsvnrevisionstring_agrhyd(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(agrhyd_build_number)
    end subroutine getsvnrevisionstring_agrhyd

    subroutine getarchitecturestring_agrhyd(stringout)
        character(*), intent(out) :: stringout

#if defined(WIN32)
        stringout = trim('Win32')
#elif defined(WIN64)
        stringout = trim('Win64')
#else
        if (c_size_t == 4) then
            stringout = trim('Linux32')
        elseif (c_size_t == 8) then
            stringout = trim('Linux64')
        else
            stringout = trim('Unknown')
        end if
#endif

    end subroutine getarchitecturestring_agrhyd

end module agrhyd_version_module
