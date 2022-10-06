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

! $Id: dfm_max25_read_nc_histories.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/tools_gpl/dfmoutput/src/dfm_max25_read_nc_histories.f90 $

!> READ_NC_HISTORIES - Subroutine to read the histories from a NetCDF file

module read_nc_histories
   use precision
   use netcdf
   use dfm_params
   implicit none

   integer :: ncid, nTimes, nNameLength, nStations

   private

   public :: read_meta_data, read_data, close_nc_his_file, read_station_names, find_stations_var

   interface read_data
      module procedure read_data_r4
      module procedure read_data_r8
   end interface read_data

   contains

   !> main subroutine to read history data from a NetCDF file
   function read_meta_data(filename, nStat) result(status)
      character(len=*), intent(in)  :: filename  !< input file name
      integer         , intent(out) :: nStat     !< number of stations found on input file
      integer                       :: status    !< function result; 0=OK

      integer :: timeID, name_lenID, stationsID, cross_name_lenID, nCrossNameLength

                                status = nf90_open(filename, nf90_nowrite, ncid)
      if (status == nf90_noerr) status = nf90_inq_dimid(ncid, "time", timeID)
      if (status == nf90_noerr) status = nf90_inq_dimid(ncid, "name_len", name_lenID)
      if (status == nf90_noerr) status = nf90_inq_dimid(ncid, "stations", stationsID)

      if (status == nf90_noerr) status = nf90_inquire_dimension(ncid, timeID, len = nTimes)
      if (status == nf90_noerr) status = nf90_inquire_dimension(ncid, name_lenID, len = nNameLength)
      if (status == nf90_noerr) status = nf90_inquire_dimension(ncid, stationsID, len = nStations)

      status = nf90_inq_dimid(ncid, "cross_section_name_len", cross_name_lenID)
      if (status == nf90_noerr) then
         status = nf90_inquire_dimension(ncid, cross_name_lenID, len = nCrossNameLength)
         nNameLength = max(nNameLength, nCrossNameLength)
      else
         status = nf90_noerr
      end if

      nStat = nStations

      if (status == nf90_noerr .and. verbose_mode) then
         write(*,*) 'dims are: ', nTimes, nNameLength, nStations
      endif
   end function read_meta_data

   !> read data from an already opened NetCDF file
   function read_data_r4(histories, name) result(status)
      real, allocatable, intent(out) :: histories(:,:)  !< output array
      character(len=*), intent(in)   :: name            !< variabele name on NetCDF file
      integer                        :: status          !< function result: 0=OK

      integer :: varid
      integer :: nVar
      integer :: t, s
      character(len=80) :: namei
      real, allocatable :: buffer(:)

      status = nf90_inquire(ncid, nVariables = nVar)
      do varid = 1, nVar
         status = nf90_inquire_variable(ncid, varId, namei)
         if (name == namei) exit
      enddo

      if (varid > nVar) then
         write(*,*) 'varname ', trim(name), ' not found.'
         stop -1
      endif

      if (status == nf90_noerr) then
         allocate(buffer(nStations), histories(nTimes, nStations), stat=status)
         if (status /= 0) call allocate_error('read_data', 'histories', nStations * (1 + nTimes))
         do t = 1, nTimes
            status = nf90_get_var(ncid, varId, buffer, start=[1,t], count=[nstations,1])
            if (status /= nf90_noerr) exit
            do s = 1, nStations
               histories(t, s) = buffer(s)
            enddo
         enddo
      endif
   end function read_data_r4

   !> read data from an already opened NetCDF file, double precision
   function read_data_r8(histories, name) result(status)
      real(kind=hp), allocatable, intent(out) :: histories(:,:)  !< output array
      character(len=*), intent(in)            :: name            !< variabele name on NetCDF file
      integer                                 :: status          !< function result: 0=OK

      integer                    :: varid
      integer                    :: nVar
      integer                    :: t, s
      character(len=80)          :: namei
      real(kind=hp), allocatable :: buffer(:)

      status = nf90_inquire(ncid, nVariables = nVar)
      do varid = 1, nVar
         status = nf90_inquire_variable(ncid, varId, namei)
         if (name == namei) exit
      enddo

      if (varid > nVar) then
         write(*,*) 'varname ', trim(name), ' not found.'
         stop -1
      endif

      if (status == nf90_noerr) then
         allocate(buffer(nStations), histories(nTimes, nStations), stat=status)
         if (status /= 0) call allocate_error('read_data', 'histories', nStations * (1 + nTimes))
         do t = 1, nTimes
            status = nf90_get_var(ncid, varId, buffer, start=[1,t], count=[nstations,1])
            if (status /= nf90_noerr) exit
            do s = 1, nStations
               histories(t, s) = buffer(s)
            enddo
         enddo
      endif
   end function read_data_r8

   !> find stations variable for a time serie
   !! result will be in most cases 'station_name' or 'cross_section_name'
   !! assumes ncid is already opened
   subroutine find_stations_var(field_name, stations_var, nStat)
      character(len=*), intent(in   ) :: field_name
      character(len=*), intent(  out) :: stations_var
      integer         , intent(  out) :: nStat

      integer :: i, status, nVar, dimids(10), ndims, varid_name, dim_stations
      character(len=64) :: name

      status = nf90_inquire(ncid, nVariables = nVar)
      varid_name = get_varid(field_name)
      status = nf90_inquire_variable(ncid, varId_name, ndims=ndims, dimids=dimids)
      dim_stations = dimids(1)

      do i = 1, nVar
         status = nf90_inquire_variable(ncid, i, name=name, ndims=ndims, dimids=dimids)
         if (index(name, '_name') > 0 .and. ndims == 2) then
            if (dimids(2) == dim_stations) then
               stations_var = name
               status = nf90_inquire_dimension(ncid, dim_stations, len = nStat)
               nStations = nStat
               return
            end if
         end if
      end do

      ! previous default:
      stations_var = 'station_name'
      nStat = nStations
   end subroutine find_stations_var

   !> read station names from an already opened NetCDF file
   function read_station_names(stations, stations_varname) result(status)
      character(len=:), allocatable, intent(out) :: stations(:)       !< output array
      character(len=*)             , intent(in ) :: stations_varname  !< variable name on NetCDF file
      integer                                    :: status            !< function result: 0=OK

      integer :: varid, i

      allocate(character(nNameLength) :: stations(nStations))
      varid = get_varid(stations_varname)

      status = nf90_get_var(ncid, varId, stations)

      do i = 1, nStations
         call convertCstring(stations(i))
      enddo
   end function read_station_names

   function get_varid(varname) result(varid)
      character(len=*), intent(in) :: varname
      integer                      :: varid  !< function result
      integer                      :: nVar, status
      character(len=80)            :: namei

      status = nf90_inquire(ncid, nVariables = nVar)
      do varid = 1, nVar
         status = nf90_inquire_variable(ncid, varId, namei)
         if (namei == varname) return
      enddo
      varid = 0
   end function get_varid

   !> close file
   function close_nc_his_file() result(status)
      integer :: status  !< function result

      status = nf90_close(ncid)
   end function close_nc_his_file

   !> convert a C string to an Fortran string,
   !! by searching for char(0) and replace it with spaces untill the end of the string
   subroutine convertCstring(text)
      character(len=*), intent(inout) :: text    !< string to be converted

      integer :: i

      do i = 1, len(text)
         if (text(i:i) == char(0)) then
            text(i:) = ' '
            exit
         endif
      enddo
   end subroutine convertCstring

   !> helper function to stop after an allocation error
   subroutine allocate_error(subname, varname, size)
      character(len=*), intent(in) :: subname  !< name of subroutine where allocation fails
      character(len=*), intent(in) :: varname  !< variable name
      integer         , intent(in) :: size     !< total size of allocation statement

      character(len=20) :: cint

      write(cint,'(i20)') size
      write(*,*) 'Allocation error in ' // subname // ' for variable ' // varname // ' with size ' // trim(adjustl(cint))
      stop -1
   end subroutine allocate_error

end module read_nc_histories
