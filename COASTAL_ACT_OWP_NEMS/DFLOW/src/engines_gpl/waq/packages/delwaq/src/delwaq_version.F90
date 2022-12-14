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

module delwaq_version_module

implicit none

    character(*),  public, parameter :: delwaq_major        = '5'
    character(*),  public, parameter :: delwaq_minor        = '10'
    character(*),  public, parameter :: delwaq_revision     = '00'
    character(*),  public, parameter :: delwaq_build_number = '141013'

    character(*),  public, parameter :: delwaq_company      = 'Deltares'
    character(*),  public, parameter :: delwaq_company_url  = 'http://www.deltares.nl'
    character(*),  public, parameter :: delwaq_program      = 'DELWAQ'

    character(*),  public, parameter :: delwaq_version      = delwaq_major//'.'//delwaq_minor//'.'//delwaq_revision//'.'//delwaq_build_number
    character(*),  public, parameter :: delwaq_version_full = 'Deltares, '//delwaq_program//' Version '//delwaq_version//', '//__DATE__//', '//__TIME__
    character(*),  public, parameter :: delwaq_version_id   = '@(#)'//delwaq_version_full

contains

    subroutine getfullversionstring_delwaq(stringout)
        character(*), intent(out) :: stringout
        integer                   :: length

        length = min(len_trim(delwaq_version_full),len(stringout))
        stringout = delwaq_version_id(5:5+length-1)
    end subroutine getfullversionstring_delwaq

end module delwaq_version_module
