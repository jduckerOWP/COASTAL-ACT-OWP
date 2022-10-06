!!  Copyright (C)  Stichting Deltares, 2012-2022.
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

!> @file
!! Module to hold all externally available data
!! - important for the connection to Delta-Shell
module delwaq2_global_data

    use delwaq2_data
    use hydroset

    implicit none

    type(delwaq_data) :: dlwqd                      !< Variable holding all internal state information

    type t_size_dlwq_state
      integer :: total
      integer :: core, pseudo, output
      integer :: conc,  other, notot, noseg
      integer :: mass, rbuf, names, timeadmin
    end type t_size_dlwq_state

    type(t_size_dlwq_state) :: size_dlwq_state

    !
    ! Intermediate arrays for dealing with the set-up
    !
    character(len=256), dimension(:),   allocatable :: argv
    character(len=256), dimension(:),   allocatable :: argv_tmp

    character(len=20),  dimension(:),   allocatable :: substance_name
    real,               dimension(:,:), allocatable :: substance_conc
    character(len=20),  dimension(:),   allocatable :: procparam_const
    character(len=20),  dimension(:),   allocatable :: procparam_param
    real,               dimension(:),   allocatable :: procparam_const_value
    real,               dimension(:,:), allocatable :: procparam_param_value
    character(len=20),  dimension(:),   allocatable :: output_param

    integer                                         :: nomult
    integer,            dimension(:,:), allocatable :: mult

    character(len=20),  dimension(:),   allocatable :: monitor_name
    integer,            dimension(:),   allocatable :: cells_per_monitor
    integer,            dimension(:),   allocatable :: monitor_cell
    integer,            dimension(:),   allocatable :: selected_cells_monitor
    character(len=20),  dimension(:),   allocatable :: transect_name
    integer,            dimension(:),   allocatable :: iqdmp_array
    integer,            dimension(:),   allocatable :: isdmp_array

    character(len=40),  dimension(:),   allocatable :: load_name
    integer,            dimension(:),   allocatable :: load_cell
    character(len=20),  dimension(:),   allocatable :: load_type
    character(len=20),  dimension(:),   allocatable :: load_type_def

    character(len=20),  dimension(:),   allocatable :: procparam_name

    integer,            dimension(50)               :: lun
    character(len=255), dimension(50)               :: lchar
    integer,            dimension(50)               :: filtype

    character(len=255), save                        :: runid = 'deltashell'

    character(len=40),  dimension(4)                :: title

    integer,            dimension(:),   allocatable :: iknmrk

    character(len=20),  dimension(:),   allocatable :: diname
    character(len=20),  dimension(:),   allocatable :: vename
    integer,            dimension(:),   allocatable :: idpnt_array
    integer,            dimension(:),   allocatable :: ivpnt_array
    real,               dimension(3)                :: disp
    real,               dimension(3)                :: aleng

    character(len=20),  dimension(:),   allocatable :: boundary_id
    character(len=40),  dimension(:),   allocatable :: boundary_name
    character(len=20),  dimension(:),   allocatable :: boundary_type
    integer,            dimension(:,:), allocatable :: ibpnt_array
    integer,            dimension(:,:), allocatable :: ipoint

    integer,            dimension(:),   allocatable :: nrftot
    integer,            dimension(:),   allocatable :: nrharm

    integer                                         :: ref_year
    integer                                         :: ref_month
    integer                                         :: ref_day
    integer                                         :: ref_hour
    integer                                         :: ref_minute
    integer                                         :: ref_second

contains

subroutine delwaq2_global_data_initialize(runid_given)
    character(len=*) :: runid_given
    integer          :: i

    lun  = (/ 14 , 15 , 16 , 17 , 18 , 19 , 20 , 21 , 22 , 23 , &
                      24 , 25 , 26 , 27 , 28 , 29 , 30 , 31 , 32 , 33 , &
                      34 , 35 , 36 , 37 , 38 , 39 , 40 , 41 , 42 , 43 , &
                      44 , 45 , 46 , 47 , 48 , 49 , 50 , 51 , 52 , 53 , &
                      54 , 55 , 56 , 57 , 58 , -1 , -1 , -1 , -1 , -1 /)

    lchar =   (/  '-delwaq03.wrk' , '-delwaq04.wrk' , &
                  '-harmonic.wrk' , '-pointers.wrk' , &
                  '-timestep.wrk' , '-gridding.wrk' , &
                  '-volumes.wrk ' , '-to_from.wrk ' , &
                  '-dispersi.wrk' , '-areas.wrk   ' , &
                  '-flows.wrk   ' , '-velocity.wrk' , &
                  '-lengthes.wrk' , '-boundary.wrk' , &
                  '-wastload.wrk' , '-function.wrk' , &
                  '-segfunc.wrk ' , '-initials.wrk' , &
                  '.mon         ' , '.dmp         ' , &
                  '.his         ' , '.map         ' , &
                  '.res         ' , '-proces.wrk  ' , &
                  '-output.wrk  ' , '.inp         ' , &
                  '             ' , '-delwaq02.wrk' , &
                  '.lst         ' , '-dlwqstrt.inc' , &
                  '-scratch1opt3' , '-scratch2opt3' , &
                  '-auxfileop1  ' , '-proces.def  ' , &
                  '.lsp         ' , '-stochi.inp  ' , &
                  '-bal.his     ' , '.hdf         ' , &
                  '.adf         ' , '-kenmerk.wrk ' , &
                  '-filenaam.wrk' , '-stat.map    ' , &
                  '-stat.mon    ' , '             ' , &
                  '             ' , '<ncgridfile> ' , &
                  '_his.nc      ' , '_bal_his.nc  ' , &
                  '_map.nc      ' , '_stat_map.nc '/)

    filtype = 0

    runid = runid_given

    do i = 1,size(lchar)
       if (lchar(i) /= ' ') then
          lchar(i) = trim(runid)//lchar(i)
       endif
    enddo
end subroutine delwaq2_global_data_initialize

subroutine delwaq2_global_data_finalize

    use waqmem

    implicit none

    integer :: i

!   first, all arrays from waqmem
    call waqmem_deallocate()

    if (allocated(argv))  deallocate(argv)

    if (allocated(substance_name))  deallocate(substance_name)
    if (allocated(substance_conc))  deallocate(substance_conc)
    if (allocated(procparam_const)) deallocate(procparam_const)
    if (allocated(procparam_param)) deallocate(procparam_param)


    if (allocated(procparam_const_value)) deallocate(procparam_const_value)

    if (allocated(mult))  deallocate(mult)

    if (allocated(monitor_name)) deallocate(monitor_name)
    if (allocated(cells_per_monitor)) deallocate(cells_per_monitor)
    if (allocated(monitor_cell)) deallocate(monitor_cell)
    if (allocated(selected_cells_monitor)) deallocate(selected_cells_monitor)
    if (allocated(transect_name)) deallocate(transect_name)
    if (allocated(iqdmp_array)) deallocate(iqdmp_array)
    if (allocated(isdmp_array)) deallocate(isdmp_array)

    if (allocated(load_name)) deallocate(load_name)
    if (allocated(load_cell)) deallocate(load_cell)
    if (allocated(load_type)) deallocate(load_type)
    if (allocated(load_type_def)) deallocate(load_type_def)

    if (allocated(procparam_name)) deallocate(procparam_name)
    if (allocated(iknmrk)) deallocate( iknmrk)

    if (allocated(diname)) deallocate(diname)
    if (allocated(vename)) deallocate( vename)
    if (allocated(idpnt_array)) deallocate(idpnt_array)
    if (allocated(ivpnt_array)) deallocate(ivpnt_array)


    if (allocated(boundary_id)) deallocate( boundary_id)
    if (allocated(boundary_name)) deallocate(boundary_name)
    if (allocated(boundary_type)) deallocate(boundary_type)
    if (allocated(ibpnt_array)) deallocate( ibpnt_array)

    if (allocated(nrftot)) deallocate( nrftot)
    if (allocated(nrharm)) deallocate( nrharm)

    ! close all files; should have been done already, but this statement
    ! nevertheless proves necessary
    do i=1,50
        if (lun(i).gt.0) then
            close(lun(i))
        endif
    enddo

end subroutine delwaq2_global_data_finalize

subroutine delwaq2_global_data_copy( dlwqd )

    type(delwaq_data) :: dlwqd

    character(len=20), dimension(1) :: dlwqname ! Template for entity names

    include 'sysn_ff.inc'   ! for noutp
    include 'sysc_ff.inc'

    integer :: iColl, max_waqfiles
    !
    ! Copy the relevant character data
    !
    if ( allocated( substance_name ) ) then
        deallocate( substance_name  )
        deallocate( procparam_const )
        deallocate( procparam_param )
    endif

    allocate( substance_name(1:notot) )
    allocate( procparam_const(1:nocons) )
    allocate( procparam_param(1:nopa) )

    substance_name  = transfer( dlwqd%chbuf(isnam:isnam+20*notot-1),  dlwqname )
    procparam_const = transfer( dlwqd%chbuf(icnam:icnam+20*nocons-1), dlwqname )
    procparam_param = transfer( dlwqd%chbuf(ipnam:ipnam+20*nopa-1),   dlwqname )

!   administrate state sizes for OpenDA use
    size_dlwq_state%notot = notot
    size_dlwq_state%noseg = noseg
    size_dlwq_state%conc  = notot*noseg
    size_dlwq_state%other = 1 ! todo: set this to zero?
    size_dlwq_state%core  = size_dlwq_state%conc + size_dlwq_state%other

    size_dlwq_state%rbuf  = size(dlwqd%rbuf)

    size_dlwq_state%mass  = notot*noseg
    size_dlwq_state%names = notot
    size_dlwq_state%timeadmin = 3

    size_dlwq_state%pseudo = size_dlwq_state%rbuf + size_dlwq_state%mass + size_dlwq_state%names + size_dlwq_state%timeadmin

    size_dlwq_state%output = 9 + 7*noutp
    size_dlwq_state%total = size_dlwq_state%core + size_dlwq_state%pseudo + size_dlwq_state%output

end subroutine delwaq2_global_data_copy

end module delwaq2_global_data

