subroutine dfreduce_gdp ( iptr, ilen, itype, ityprd, gdp )
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
!  $Id: dfreduce_gdp.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/dfreduce_gdp.f90 $
!!--description-----------------------------------------------------------------
!
!   Performs a global reduction of type ITYPRD on
!   array IPTR of type ITYPE to collect values from
!   all processes
!
!!--pseudo code and references--------------------------------------------------
!
!   wrapper for MPI_ALLREDUCE
!
!
!!--declarations----------------------------------------------------------------
    use flow2d3d_timers
    use globaldata
    !
    implicit none
    !
    type(globdat), target    :: gdp
!
! Global variables
!
    integer             :: iptr   ! pointer to first element of array to be collect
    integer, intent(in) :: ilen   ! length of array to be collect
    integer, intent(in) :: itype  ! type of data
    integer, intent(in) :: ityprd ! type of reduction
!
! Local variables
!
    integer, pointer    :: lundia
    !
    logical             :: error  ! error flag
    character(80)       :: msgstr ! error message
!
!! executable statements -------------------------------------------------------
!
    lundia => gdp%gdinout%lundia
    !
    call timer_start(timer_dfreduce, gdp)
    !
    call dfreduce ( iptr, ilen, itype, ityprd, error, msgstr )
    if ( error ) then
       call prterr(lundia, 'U021', trim(msgstr))
       call d3stop(1, gdp)
    endif
    !
    call timer_stop(timer_dfreduce, gdp)
end subroutine dfreduce_gdp
