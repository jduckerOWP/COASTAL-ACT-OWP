module deltares_common_version_module
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
!  $Id: deltares_common_version.F90.svn 6921 2017-01-12 15:07:25Z jeuke_ml $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/utils_lgpl/deltares_common/packages/deltares_common/src/deltares_common_version.F90.svn $

implicit none

    character(*),  public, parameter :: deltares_common_major        = '1'
    character(*),  public, parameter :: deltares_common_minor        = '00'
    character(*),  public, parameter :: deltares_common_revision     = '00'
    character(*),  public, parameter :: deltares_common_build_number = '141013M'

    character(*),  public, parameter :: deltares_common_company      = 'Deltares'
    character(*),  public, parameter :: deltares_common_company_url  = 'http://www.deltares.nl'
    character(*),  public, parameter :: deltares_common_program      = 'DELTARES_COMMON'

    character(*),  public, parameter :: deltares_common_version      = deltares_common_major//'.'//deltares_common_minor//'.'//deltares_common_revision//'.'//deltares_common_build_number
    character(*),  public, parameter :: deltares_common_version_full = 'Deltares, '//deltares_common_program//' Version '//deltares_common_version//', '//__DATE__//', '//__TIME__
    character(*),  public, parameter :: deltares_common_version_id   = '@(#)'//deltares_common_version_full
    character(*),  public, parameter :: deltares_common_source_code  = '@(#) $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/utils_lgpl/deltares_common/packages/deltares_common/src/deltares_common_version.F90.svn $'//char(0)

contains

    subroutine getfullversionstring_deltares_common(stringout)
        character(*), intent(out) :: stringout
        integer                   :: length

        length = min(len_trim(deltares_common_version_full),len(stringout))
        stringout = deltares_common_version_id(5:5+length-1)
    end subroutine getfullversionstring_deltares_common

end module deltares_common_version_module
