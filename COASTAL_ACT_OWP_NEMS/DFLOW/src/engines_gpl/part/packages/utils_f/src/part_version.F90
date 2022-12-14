!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2017.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
!  $Id: io_netcdf_version.F90.svn 6659 2016-10-18 09:04:07Z dam_ar $
!  $HeadURL$

module part_version_module

implicit none

    character(*),  public, parameter :: part_major        = '3'
    character(*),  public, parameter :: part_minor        = '81'
    character(*),  public, parameter :: part_revision     = '00'
    character(*),  public, parameter :: part_build_number = '141013M'

    character(*),  public, parameter :: part_company      = 'Deltares'
    character(*),  public, parameter :: part_company_url  = 'http://www.deltares.nl'
    character(*),  public, parameter :: part_program      = 'PART'

    character(*),  public, parameter :: part_version      = part_major//'.'//part_minor//'.'//part_revision//'.'//part_build_number
    character(*),  public, parameter :: part_version_full = 'Deltares, '//part_program//' Version '//part_version//', '//__DATE__//', '//__TIME__
    character(*),  public, parameter :: part_version_id   = '@(#)'//part_version_full

contains

    subroutine getfullversionstring_part(stringout)
        character(*), intent(out) :: stringout
        integer                   :: length

        length = min(len_trim(part_version_full),len(stringout))
        stringout = part_version_id(5:5+length-1)
    end subroutine getfullversionstring_part

end module part_version_module
