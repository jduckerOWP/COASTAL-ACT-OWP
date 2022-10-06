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
!! General interface routines to DELWAQ:
!! The routines are accessible from C/C++
!!<


!> Implementation of the routines that interface to DELWAQ
module waq_omi_priv

    use waq_omi_utils
    use waq_omi_constants
    use delwaq2_global_data

    implicit none

    ! // here: all ei-enumerations!!!!


contains

!> Set a value
logical function SetValuePriv(dlwqtype, parid, locid, values, operation)
    implicit none

    integer, intent(in)              :: dlwqtype         !< Type of parameter to be set
    integer, intent(in)              :: parid            !< Index of the parameter
    integer, intent(in)              :: locid            !< Index of the parameter
    real, dimension(:), intent(in)   :: values           !< Value to be used in the operation
    integer, intent(in)              :: operation        !< Operation to apply

    include 'sysn_ff.inc'
    include 'sysa_ff.inc'

    integer, dimension(6)            :: idx
    logical                          :: success

    !
    ! Check the arguments
    !
    success = .true.
    call CheckParameterId( dlwqtype, parid, success )
    call CheckLocationId(  dlwqtype, locid, success )
    call CheckOperation(   operation, success )

    if ( .not. success ) then
        SetValuePriv = .false.
        return
    endif

    idx = DetermineIndex( dlwqtype, parid, locid )

    !
    ! Sanity check:
    ! The size of the array "values" must be equal to
    ! the number of items stored in "idx"
    !
    if ( size(values) /= idx(3) ) then
        write(*,*) 'SetValues: Error - inconsistent number of values'
        write(*,*) '           Number of substances: ', notot
        write(*,*) '           Number of segments:   ', noseg
        write(*,*) '           Number of values:     ', size(values)
        write(*,*) '           Should be 1, ', notot*noseg, ' or ', noseg
        SetValuePriv = .false.
        return
    endif

    call StoreOperation( idx, size(values), values, operation )

    SetValuePriv = .true.

end function SetValuePriv

!> Determine the exact index in the overall rbuf array
!!
!! The return value is an array of three data: index, step
!! and potential number of elements that is affected
function DetermineIndex( dlwqtype, parid, locid )

    integer, intent(in)   :: dlwqtype
    integer, intent(in)   :: parid
    integer, intent(in)   :: locid
    integer, dimension(6) :: DetermineIndex

    integer               :: idx
    integer               :: step
    integer               :: number
    integer               :: idxmass
    integer               :: idxvol
    integer               :: volcorr

    include 'sysa_ff.inc'
    include 'sysn_ff.inc'

    idxmass = -1
    idxvol  = -1
    volcorr = 1

    select case( dlwqtype )
        case ( DLWQ_CONSTANT )
            idx    = icons+parid-1
            step   = 1
            number = 1

        case ( DLWQ_PARAMETER )
            if ( locid /= ODA_ALL_SEGMENTS ) then
                idx    = iparm+parid-1+(locid-1)*nopa
                step   = nopa
                number = 1
            else
                idx    = iparm+parid-1
                step   = nopa
                number = noseg
            endif

        case ( DLWQ_CONCENTRATION )
            if ( locid /= ODA_ALL_SEGMENTS ) then
                idx     = iconc+parid-1+(locid-1)*notot
                step    = notot
                number  = 1
                idxmass = imass+parid-1+(locid-1)*notot
                idxvol  = ivol+locid-1
            else
                idx     = iconc+parid-1
                step    = notot
                number  = noseg
                idxmass = imass+parid-1
                idxvol  = ivol
            endif


        case (DLWQ_ALL_CONCENTRATIONS)
           ! all segments and all substances are adressed at once.
                idx     = iconc
                step    = 1
                number  = noseg*notot
                idxmass = imass
                idxvol  = ivol
                volcorr = notot

        case ( DLWQ_BOUNDARY_VALUE )
            idx    = ibset+parid-1+(locid-1)*nosys
            step   = nosys
            number = nobnd

        case ( DLWQ_BOUNDARY_SECTION )
            write(*,*) 'Not implemented yet!'
            call srstop(1)

        case ( DLWQ_DISCHARGE )
            idx    = iwste+parid+(locid-1)*(notot+1)
            step   = notot
            number = nowst

        case ( DLWQ_MONITOR_POINT )
            idx     = iconc+parid-1+(locid-1)*notot
            step    = notot
            number  = 1

        case default
            idx     = 0
            step    = 1
            number  = 0
    end select

    DetermineIndex(1) = idx
    DetermineIndex(2) = step
    DetermineIndex(3) = number
    DetermineIndex(4) = idxmass
    DetermineIndex(5) = idxvol
    DetermineIndex(6) = volcorr

end function DetermineIndex

!> Store the operation and the new value
!!
!! The operation should actually be applied within a suitable point
!! in the computational cycle, so it is stored in a buffer for later use
!  note: new_values should be an array of size <number_values>
!
subroutine StoreOperation( index, number_values, new_value, operation )

    integer, dimension(:), intent(in)     :: index           !< Index into the rbuf array
    integer, intent(in)                   :: number_values   !< Number of values
    real, dimension(:), intent(in)        :: new_value       !< Array of new values or modification values
    integer, intent(in)                   :: operation       !< Operation to be performed

    integer                               :: pos

    pos = FreeOperationEntry()

    dlwqd%operation(pos)%index         = index
    dlwqd%operation(pos)%number_values = number_values

    if ( number_values > 1 ) then
        if ( associated( dlwqd%operation(pos)%new_value ) ) then
            if ( size(dlwqd%operation(pos)%new_value) /= number_values ) then
                deallocate( dlwqd%operation(pos)%new_value )
                allocate( dlwqd%operation(pos)%new_value(1:number_values) )
            endif
        else
           allocate( dlwqd%operation(pos)%new_value(1:number_values) )
        endif
        dlwqd%operation(pos)%new_value = new_value   ! so size of new_value is number_values!
    else
        dlwqd%operation(pos)%new_scalar = new_value(1)
    endif

    dlwqd%operation(pos)%operation = operation
    dlwqd%number_operations        = pos


end subroutine StoreOperation

!> Find or create a new entry in the buffer of operations
integer function FreeOperationEntry()

    integer :: i
    integer :: pos
    type(operation_data), dimension(:), pointer :: new_operation

    if ( .not. associated(dlwqd%operation) ) then
        allocate( dlwqd%operation(10) )
        dlwqd%operation%operation = DLWQ_FREE
    endif

    pos = -1
    do i = 1,size(dlwqd%operation)
        if ( dlwqd%operation(i)%operation == DLWQ_FREE ) then
            pos = i
            exit
        endif
    enddo

    if ( pos == -1 ) then
        allocate( new_operation( 2*size(dlwqd%operation) ) )

        pos = 1 + size(dlwqd%operation)
        new_operation%operation                = DLWQ_FREE
        new_operation(1:size(dlwqd%operation)) = dlwqd%operation

        deallocate( dlwqd%operation ) ! Not the new_value arrays! We still need those

        dlwqd%operation => new_operation
    endif

    FreeOperationEntry = pos

end function FreeOperationEntry

!> Set the new value, based on the operation
!!
!! The operation should actually be applied within a suitable point
!! in the computational cycle, so it is stored in a buffer for later use
subroutine SetNewValue( value, new_value, operation )

    real, intent(inout)    :: value
    real, intent(in)       :: new_value
    integer, intent(in)    :: operation

    select case ( operation )
        case ( DLWQ_SET )
            value = new_value

        case ( DLWQ_ADD )
            value = value + new_value

        case ( DLWQ_MULTIPLY )
            value = value * new_value

        case default
            ! Do nothing
    end select

end subroutine SetNewValue


!> Retrieve a value
logical function GetValuePriv(type, parid, locid, value)

    integer, intent(in)              :: type             !< Type of parameter to be set
    integer, intent(in)              :: parid            !< Index of the parameter
    integer, intent(in)              :: locid            !< Index of the location
    real, dimension(:), intent(out)  :: value            !< Value to be used in the operation

    integer, dimension(6)            :: idx
    logical                          :: success

    !
    ! Check the arguments
    !
    success = .true.
    call CheckParameterId( type, parid, success )
    call CheckLocationId(  type, locid, success )

    if ( .not. success ) then
        GetValuePriv = .false.
        return
    endif

    idx = DetermineIndex( type, parid, locid )

    value = dlwqd%rbuf(idx(1):idx(1)+(idx(3)-1)*idx(2):idx(2))
    GetValuePriv = .true.

end function GetValuePriv


!> Check that the parameter ID is valid
subroutine CheckParameterId( dlwqtype, parid, success )

    integer, intent(in)    :: dlwqtype
    integer, intent(in)    :: parid
    logical, intent(inout) :: success

    include 'sysn_ff.inc'

    select case ( dlwqtype )
        case ( DLWQ_CONSTANT )
            success = parid >= 1 .and. parid <= nocons

        case ( DLWQ_PARAMETER )
            success = parid >= 1 .and. parid <= nopa

        case ( DLWQ_CONCENTRATION )
            success = parid >= 1 .and. parid <= notot

        case ( DLWQ_BOUNDARY_VALUE )
            success = parid >= 1 .and. parid <= nosys

        case ( DLWQ_BOUNDARY_SECTION )
            write(*,*) 'Not implemented yet!'
            call srstop(1)

        case ( DLWQ_DISCHARGE )
            success = parid >= 1 .and. parid <= notot   ! Allow for Flow rate?

        case ( DLWQ_MONITOR_POINT )
            success = parid >= 1 .and. parid <= notot

        case (DLWQ_ALL_CONCENTRATIONS)
            success = parid == -1

        case default
            success = .false.
    end select

end subroutine CheckParameterId

!> Check that the location ID is valid
subroutine CheckLocationId(  dlwqtype, locid, success )

    integer, intent(in)    :: dlwqtype
    integer, intent(in)    :: locid
    logical, intent(inout) :: success

    include 'sysn_ff.inc'

    select case ( dlwqtype )
        case ( DLWQ_CONSTANT )
            success = .true. ! locid ignored

        case ( DLWQ_PARAMETER, DLWQ_CONCENTRATION, DLWQ_ALL_CONCENTRATIONS )
            success = (locid >= 1 .and. locid <= noseg) .or. (locid == ODA_ALL_SEGMENTS)       ! Allow for "all segments"?

        case ( DLWQ_BOUNDARY_VALUE )
            success = locid >= 1 .and. locid <= nobnd

        case ( DLWQ_BOUNDARY_SECTION )
            write(*,*) 'Not implemented yet!'
            call srstop(1)

        case ( DLWQ_DISCHARGE )
            success = locid >= 1 .and. locid <= nowst

        case ( DLWQ_MONITOR_POINT )
            success = locid >= 1 .and. locid <= noseg

        case default
            success = .false.
    end select

end subroutine CheckLocationId

!> Check that the operation is valid
subroutine CheckOperation(   operation, success )

    integer, intent(in)    :: operation
    logical, intent(inout) :: success

    select case ( operation )
        case ( DLWQ_SET, DLWQ_ADD, DLWQ_MULTIPLY )
            success = .true.

        case default
            success = .false.
    end select

end subroutine CheckOperation

!> Retrieve number of locations (of given type)
integer function GetLocationCountPriv( type )

    integer, intent(in)    :: type

    include 'sysn_ff.inc'

    select case ( type )
        case ( DLWQ_CONSTANT )
            GetLocationCountPriv = 1

        case ( DLWQ_PARAMETER )
            GetLocationCountPriv = noseg

        case ( DLWQ_CONCENTRATION )
            GetLocationCountPriv = noseg

        case ( DLWQ_DISCHARGE )
            GetLocationCountPriv = nowst

        case ( DLWQ_BOUNDARY_VALUE )
            GetLocationCountPriv = nobnd

        case ( DLWQ_BOUNDARY_SECTION )
            write(*,*) 'Not implemented yet!'
            call srstop(1)

        case ( DLWQ_MONITOR_POINT )
            GetLocationCountPriv = nodump

        case default
            GetLocationCountPriv = 0
    end select

end function GetLocationCountPriv

!> Retrieve indices of locations (of given type)
integer function GetLocationIndicesPriv( type, idsSize, ids )

    integer, intent(in)                :: type
    integer, intent(in)                :: idsSize
    integer, dimension(:), intent(out) :: ids

    integer                            :: count
    integer                            :: i

    include 'sysn_ff.inc'

    count = GetLocationCountPriv( type )

    if ( idsSize /= count ) then
        ids   = 0
        GetLocationIndicesPriv = -1
    else
        ids   = (/ (i ,i=1,count) /)
        GetLocationIndicesPriv = 0
    endif

end function GetLocationIndicesPriv

!> Retrieve number of items (of given type)
integer function GetItemCountPriv( type )

    integer, intent(in)    :: type

    include 'sysn_ff.inc'

    select case ( type )
        case ( DLWQ_CONSTANT )
            GetItemCountPriv = nocons

        case ( DLWQ_PARAMETER )
            GetItemCountPriv = nopa

        case ( DLWQ_CONCENTRATION )
            GetItemCountPriv = notot

        case ( DLWQ_DISCHARGE )
            GetItemCountPriv = notot

        case ( DLWQ_BOUNDARY_VALUE )
            GetItemCountPriv = nosys

        case ( DLWQ_BOUNDARY_SECTION )
            GetItemCountPriv = nosys

        case ( DLWQ_ACTIVE_SUBSTANCES )
            GetItemCountPriv = nosys

        case ( DLWQ_ALL_SUBSTANCES )
            GetItemCountPriv = notot

        case ( DLWQ_MONITOR_POINT )
            GetItemCountPriv = notot

        case default
            GetItemCountPriv = 0
    end select

end function GetItemCountPriv

!> Retrieve index of items (of given type)
integer function GetItemIndexPriv( dlwqtype, name )

    integer, intent(in)             :: dlwqtype
    character(len=*), intent(in)    :: name

    integer                         :: idx

    select case ( dlwqtype )
        case ( DLWQ_CONSTANT )
            call find_index( name, procparam_const, idx )

        case ( DLWQ_PARAMETER )
            call find_index( name, procparam_param, idx )

        case ( DLWQ_CONCENTRATION )
            call find_index( name, substance_name, idx )

        case ( DLWQ_DISCHARGE )
            call find_index( name, substance_name, idx )

        case ( DLWQ_BOUNDARY_VALUE )
            call find_index( name, substance_name, idx )

        case ( DLWQ_BOUNDARY_SECTION )
            call find_index( name, substance_name, idx )

        case default
            idx = 0
    end select

    GetItemIndexPriv = idx

end function GetItemIndexPriv

!> Retrieve the name of items (of given type)
integer function GetItemNamePriv( type, idx, name )

    integer, intent(in)             :: type
    character(len=*), intent(out)   :: name
    integer                         :: idx

    if ( idx < 1 .or. idx > GetItemCountPriv( type ) ) then
        GetItemNamePriv = -1
        name             = '?'
    endif

    select case ( type )
        case ( DLWQ_CONSTANT )
            name = procparam_const(idx)

        case ( DLWQ_PARAMETER )
            name = procparam_param(idx)

        case ( DLWQ_CONCENTRATION )
            name = substance_name(idx)

        case ( DLWQ_DISCHARGE )
            name = substance_name(idx)

        case ( DLWQ_BOUNDARY_VALUE )
            name = substance_name(idx)

        case ( DLWQ_BOUNDARY_SECTION )
            name = substance_name(idx)

        case default
            name = '?'
    end select

    GetItemNamePriv = 0

end function GetItemNamePriv

!> Retrieve index of location (of given type)
integer function GetLocationIndexPriv( type, name )

    integer, intent(in)             :: type
    character(len=*), intent(in)    :: name

    integer                         :: idx

    select case ( type )
        case ( DLWQ_DISCHARGE )
            call find_index( name, load_name, idx )

        case ( DLWQ_BOUNDARY_VALUE )
            call find_index( name, boundary_name, idx )

        case ( DLWQ_BOUNDARY_SECTION )
            write(*,*) 'Not implemented yet!'
            call srstop(1)

        case ( DLWQ_MONITOR_POINT )
            call find_index( name, monitor_name, idx )

        case default
            idx = 0
    end select

    GetLocationIndexPriv = idx

end function GetLocationIndexPriv

!> Get the time parameters for the computation
subroutine GetTimeParameters( start, stop, step, current )

    implicit none

    real(kind=kind(1.0d0)), intent(out)                :: start
    real(kind=kind(1.0d0)), intent(out)                :: stop
    real(kind=kind(1.0d0)), intent(out)                :: step
    real(kind=kind(1.0d0)), intent(out)                :: current

    include 'sysi_ff.inc'

    start   = dlwqd%otime + itstrt / dlwqd%tscale
    stop    = dlwqd%otime + itstop / dlwqd%tscale
    step    =               idt    / dlwqd%tscale
    current = dlwqd%otime + dlwqd%itime  / dlwqd%tscale

end subroutine GetTimeParameters

end module waq_omi_priv

integer function Count_Values(partype, parid, loctype, locid)
    !DEC$ ATTRIBUTES DLLEXPORT::Count_Values
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'COUNT_VALUES' :: Count_Values

    use waq_omi_priv
    use waq_omi_constants

    implicit none

    include 'sysa_ff.inc'
    include 'sysn_ff.inc'

    integer, intent(in)                   :: parid            !< Index of the parameter
    integer, intent(in)                   :: partype          !< Type of parameter to be set
    integer, intent(in)                   :: locid            !< Index of the location
    integer, intent(in)                   :: loctype          !< Type of the location

    integer                               :: dlwqtype
    integer                               :: count


    call odatype_2_dlwq(partype, loctype, dlwqtype)
    if (dlwqtype == DLWQ_CONCENTRATION .or.           &
        dlwqtype == DLWQ_PARAMETER) then
        count = noseg
    elseif (dlwqtype == DLWQ_ALL_CONCENTRATIONS) then
        count = noseg*notot
    else
        count = 1
    endif
    ! return value:
    Count_Values = count

end function Count_Values


!> Set the current value of a substance or process parameter:
!! This function provides a general interface to the state variables
!! and computational parameters.
!!
!! The type should be one of the following:
!! \li DLWQ_CONSTANT: set the value of a "constant" process parameter (location-independent)
!! \li DLWQ_PARAMETER: set the value of a "parameter" process parameter
!! \li DLWQ_CONCENTRATION: set the value of a substance (or other state variable)
!! \li DLWQ_DISCHARGE: set the value for the discharge (mass per time) of a substance
!! \li DLWQ_BOUNDARYVALUE: set the value for the boundary condition of a substance
!!
!! The parameter ID should correspond to the correct type, as should the location ID.
!! (For constants the location ID is ignored)
!!
!! The operation can be:
!! \li DLWQ_SET: simply replace the original value by the new value
!! \li DLWQ_ADD: add the given value to the original value
!! \li DLWQ_MULTIPLY: multiply the original value by the given value
!!
!! Note: Specifically for OpenDA
integer function Set_Values(partype, parid, loctype, locid, operation, number, values)

    !DEC$ ATTRIBUTES DLLEXPORT::Set_Values
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SET_VALUES' :: Set_Values

    use waq_omi_priv
    use waq_omi_constants

    implicit none

    integer, intent(in)                   :: parid            !< Index of the parameter
    integer, intent(in)                   :: partype          !< Type of parameter to be set
    integer, intent(in)                   :: locid            !< Index of the location or ODA_ALL_SEGMENTS
    integer, intent(in)                   :: loctype          !< Type of location to be set
    integer, intent(in)                   :: operation        !< Operation to apply
    integer, intent(in)                   :: number           !< Number of values
    double precision, dimension(number), intent(in)   :: values  !< Value(s) to be used in the operation

    real, dimension(:), allocatable       :: r_values

    integer                               :: idx
    integer                               :: locid_
    integer                               :: parid_
    integer                               :: dlwqtype
    logical                               :: success

    call odatype_2_dlwq(partype, loctype, dlwqtype)
    parid_ = parid
    if ( dlwqtype /= ODA_PAR_TYPE_GLOBALPAR .and. loctype == ODA_LOC_TYPE_ALL_SEGMENTS ) then
        locid_ = ODA_ALL_SEGMENTS
    else
        locid_ = locid
    endif
    if (dlwqtype == DLWQ_ALL_CONCENTRATIONS) then
       locid_ = ODA_ALL_SEGMENTS
       parid_ = -1  ! TODO: ????
    endif

    allocate( r_values(number) )
    r_values = values

    success   = SetValuePriv( dlwqtype, parid_, locid_, r_values, operation )

    deallocate( r_values )

    Set_Values = merge( 1, 0, success )
end function Set_Values


!> Set the current value of a substance or process parameter:
!! This function provides a general interface to the state variables
!! and computational parameters.
!!
!! The type should be one of the following:
!! \li DLWQ_CONSTANT: set the value of a "constant" process parameter (location-independent)
!! \li DLWQ_PARAMETER: set the value of a "parameter" process parameter
!! \li DLWQ_CONCENTRATION: set the value of a substance (or other state variable)
!! \li DLWQ_DISCHARGE: set the value for the discharge (mass per time) of a substance
!! \li DLWQ_BOUNDARYVALUE: set the value for the boundary condition of a substance
!!
!! The parameter ID should correspond to the correct type, as should the location ID.
!! (For constants the location ID is ignored)
!!
!! The operation can be:
!! \li DLWQ_SET: simply replace the original value by the new value
!! \li DLWQ_ADD: add the given value to the original value
!! \li DLWQ_MULTIPLY: multiply the original value by the given value
!!
!! Note: this routine is meant for general interfacing - it assumes you use the
!! DELWAQ codes for the arguments dlwqtype and operation, as well as a single-precision
!! array for the values.
!!
!! Note: the number of values must match the number of values expected for the type
!! of data.
!!
integer function Set_Values_General(dlwqtype, parid, locid, operation, number, values)

    !DEC$ ATTRIBUTES DLLEXPORT::Set_Values_General
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SET_VALUES_GENERAL' :: Set_Values_General

    use waq_omi_priv
    use waq_omi_constants

    implicit none

    integer, intent(in)                   :: dlwqtype         !< Type of parameter/location to be set
    integer, intent(in)                   :: parid            !< Index of the parameter
    integer, intent(in)                   :: locid            !< Index of the location
    integer, intent(in)                   :: operation        !< Operation to apply
    integer, intent(in)                   :: number           !< Number of values
    real, dimension(number), intent(in)   :: values           !< Value(s) to be used in the operation

    logical                               :: success

    success   = SetValuePriv( dlwqtype, parid, locid, values, operation )
    Set_Values_General = merge( 1, 0, success )
end function Set_Values_General

!> Retrieve the current value of a substance or process parameter:
!! This function provides a general interface to retrieving the state variables
!! and computational parameters.
!!
!! The type should be one of the following:
!! \li DLWQ_CONSTANT: set the value of a "constant" process parameter (location-independent)
!! \li DLWQ_PARAMETER: set the value of a "parameter" process parameter
!! \li DLWQ_CONCENTRATION: set the value of a substance (or other state variable)
!! \li DLWQ_DISCHARGE: set the value for the discharge (mass per time) of a substance
!! \li DLWQ_BOUNDARYVALUE: set the value for the boundary condition of a substance
!!
!! The parameter ID should correspond to the correct type, as should the location ID.
!! (For constants the location ID is ignored)
integer function Get_Values(partype, parid, loctype, locid, number, values)

    !DEC$ ATTRIBUTES DLLEXPORT::Get_Values
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GET_VALUES' :: Get_Values

    use waq_omi_priv
    use waq_omi_constants
!    use m_delwaq_2_openda ! Only for current_instance

    implicit none

    integer, intent(in)              :: partype          !< Type of parameter to be set
    integer, intent(in)              :: parid            !< Index of the parameter
    integer, intent(in)              :: loctype          !< Type of location to be set
    integer, intent(in)              :: locid            !< Index of the location
    integer, intent(in)              :: number           !< Size of array values
    double precision, dimension(number),intent(out):: values         !< Value to be used in the operation

    real, dimension(:), allocatable  :: r_values
    integer                          :: idx
    integer                          :: dlwqtype
    logical                          :: success
    real(kind=kind(1.0d0))           :: currentTime
    integer                          :: dummy
    integer, external                :: GetWQCurrentTime

    allocate( r_values(number) )

    call odatype_2_dlwq(partype, loctype, dlwqtype)
    success  = GetValuePriv( dlwqtype, parid, locid, r_values )
    values = r_values

    deallocate( r_values )

    Get_Values = merge( 1, 0, success )

    dummy = GetWQCurrentTime(currentTime)
   ! write(*,*) 'DELWAQ: Get_Values ', current_instance, parid, locid, number, values(1), &
   !     ' time = ', GetWQCurrentTime(currentTime)

end function Get_Values


!> Interface to CheckParameterId in the waq_omi_priv module for testing purposes
subroutine CheckParameterId(type, parid, success)

    !DEC$ ATTRIBUTES DLLEXPORT::CheckParameterId
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'CHECKPARAMETERID' :: CheckParameterId

    use waq_omi_priv, TestCheckParameterId => CheckParameterId

    implicit none

    integer, intent(in)              :: type
    integer, intent(in)              :: parid
    logical, intent(out)             :: success

    call TestCheckParameterId( type, parid, success )

end subroutine CheckParameterId


!> Interface to SetNewValue in the waq_omi_priv module for testing purposes
subroutine SetNewValue(value, new_value, operation)

    !DEC$ ATTRIBUTES DLLEXPORT::SetNewValue
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETNEWVALUE' :: SetNewValue

    use waq_omi_priv, TestSetNewValue => SetNewValue

    implicit none

    real, intent(inout)              :: value
    real, intent(in)                 :: new_value
    integer, intent(in)              :: operation

    call TestSetNewValue( value, new_value, operation )

end subroutine SetNewValue

!> Interface to get DLWQD from the library for testing purposes
subroutine GetDlwqd( dlwqd_copy )

    use delwaq2_global_data

    !DEC$ ATTRIBUTES DLLEXPORT::GetDlwqd
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETDLWQD' :: GetDlwqd

    type(delwaq_data) :: dlwqd_copy

    dlwqd_copy = dlwqd

end subroutine GetDlwqd

!> Interface to fill DLWQD from the library for testing purposes
subroutine SetDlwqd( dlwqd_copy )

    use delwaq2_global_data

    !DEC$ ATTRIBUTES DLLEXPORT::SetDlwqd
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETDLWQD' :: SetDlwqd

    type(delwaq_data) :: dlwqd_copy

    dlwqd = dlwqd_copy

end subroutine SetDlwqd

!> Interface to set a number of variables in the COMMON blocks for testing purposes
!!
!! <i>Note:</i> This is necessary because the COMMON blocks in the DLL are <i>different</i>
!! from the ones in the test program
subroutine SetCommonVars( icons_, iparm_, iconc_, ibset_, iwste_, nosys_, notot_, nocons_, nopa_, noseg_, nowst_, nobnd_ )

    !DEC$ ATTRIBUTES DLLEXPORT::SetCommonVars
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETCOMMONVARS' :: SetCommonVars

    implicit none

    include 'sysa_ff.inc'
    include 'sysn_ff.inc'

    integer :: icons_, iparm_, iconc_, ibset_, iwste_, nosys_, notot_, nocons_, nopa_, noseg_, nowst_, nobnd_

    icons  = icons_
    iparm  = iparm_
    iconc  = iconc_
    ibset  = ibset_
    iwste  = iwste_
    nosys  = nosys_
    notot  = notot_
    nototp = notot_   ! Particles not supported yet
    nocons = nocons_
    nopa   = nopa_
    noseg  = noseg_
    nowst  = nowst_
    nobnd  = nobnd_

end subroutine SetCommonVars

!> Interface to StoreOperation in the waq_omi_priv module for testing purposes
subroutine StoreOperation( index, number, new_value, operation )

    !DEC$ ATTRIBUTES DLLEXPORT::StoreOperation
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'STOREOPERATION' :: StoreOperation

    use waq_omi_priv, TestStoreOperation => StoreOperation

    implicit none

    integer, dimension(3), intent(in)    :: index
    integer, intent(in)                  :: number
    real, dimension(number), intent(in)  :: new_value
    integer, intent(in)                  :: operation

    call TestStoreOperation( index, number, new_value, operation )

end subroutine StoreOperation

!> Interface to apply_operations in the delwaq2_data module for testing purposes
subroutine test_apply_operations( dlwqd )

    !DEC$ ATTRIBUTES DLLEXPORT::test_apply_operations
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'TEST_APPLY_OPERATIONS' :: test_apply_operations

    use delwaq2_data

    implicit none

    type(delwaq_data) :: dlwqd

    call apply_operations( dlwqd )

end subroutine test_apply_operations


!> Retrieve number of locations (of given type)
integer function GetLocationCount( odatype )

    !DEC$ ATTRIBUTES DLLEXPORT::GetLocationCount
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETLOCATIONCOUNT' :: GetLocationCount

    use waq_omi_priv

    implicit none

    integer, intent(in)    :: odatype

    integer :: dlwqtype

    call odatype_2_dlwq(ODA_PAR_TYPE_SUBSTANCE, odatype, dlwqtype)
    GetLocationCount = GetLocationCountPriv( dlwqtype )

end function GetLocationCount

!> Retrieve number of items (of given type)
integer function GetItemCount( type )

    !DEC$ ATTRIBUTES DLLEXPORT::GetItemCount
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETITEMCOUNT' :: GetItemCount

    use waq_omi_priv

    implicit none

    integer, intent(in)    :: type

    GetItemCount = GetItemCountPriv( type )

end function GetItemCount

!> Retrieve index of a location (of given type)
integer function GetLocationId( type, name )

    !DEC$ ATTRIBUTES DLLEXPORT::GetLocationId
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETLOCATIONID' :: GetLocationId

    use waq_omi_priv

    implicit none

    integer, intent(in)             :: type
    character(len=*), intent(in)    :: name

    GetLocationId = GetLocationIndexPriv( type, name )

end function GetLocationId

!> Retrieve index of a location (of given type)
integer function GetLocationIds( type, idsSize, ids )

    !DEC$ ATTRIBUTES DLLEXPORT::GetLocationIds
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETLOCATIONIDS' :: GetLocationIds

    use waq_omi_priv

    implicit none

    integer, intent(in)                      :: type
    integer, intent(in)                      :: idsSize
    integer, dimension(idsSize), intent(out) :: ids

    GetLocationIds = GetLocationIndicesPriv( type, idsSize, ids )

end function GetLocationIds

!> Retrieve index of an item (of given type)
integer function GetItemIndex( type, name )

    !DEC$ ATTRIBUTES DLLEXPORT::GetItemIndex
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETITEMINDEX' :: GetItemIndex

    use waq_omi_priv

    implicit none

    integer, intent(in)             :: type
    character(len=*), intent(in)    :: name

    GetItemIndex = GetItemIndexPriv( type, name )

end function GetItemIndex

!> Retrieve name of a location (of given type)
integer function GetLocationName( type, idx, name )

    !DEC$ ATTRIBUTES DLLEXPORT::GetLocationName
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETLOCATIONNAME' :: GetLocationName

    use waq_omi_priv

    implicit none

    integer, intent(in)              :: type            !< Type of location
    integer, intent(in)              :: idx             !< Index of the location
    character(len=*), intent(out)    :: name            !< Name of the location (if successful)

   ! GetLocationName = GetLocationNamePriv( type, idx, name )
    name = 'TODO'
    GetLocationName = 1

end function GetLocationName

!> Retrieve name of an item (of given type)
integer function GetItemName( type, idx, name )

    !DEC$ ATTRIBUTES DLLEXPORT::GetItemName
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETITEMNAME' :: GetItemName

    use waq_omi_priv

    implicit none

    integer, intent(in)              :: type            !< Type of item
    integer, intent(in)              :: idx             !< Index of item
    character(len=*), intent(out)    :: name            !< Name of the item (if successful)

    GetItemName = GetItemNamePriv( type, idx, name )

end function GetItemName

!> Get the number of active substances
integer function GetActiveSubstancesCount( )

    !DEC$ ATTRIBUTES DLLEXPORT::GetActiveSubstancesCount
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETACTIVESUBSTANCESCOUNT' :: GetActiveSubstancesCount

    use waq_omi_priv

    implicit none

    GetActiveSubstancesCount = GetItemCountPriv( DLWQ_ACTIVE_SUBSTANCES )

end function GetActiveSubstancesCount

!> Get the number of all substances
integer function GetTotalSubstancesCount( )

    !DEC$ ATTRIBUTES DLLEXPORT::GetTotalSubstancesCount
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETTOTALSUBSTANCESCOUNT' :: GetTotalSubstancesCount

    use waq_omi_priv

    implicit none

    GetTotalSubstancesCount = GetItemCountPriv( DLWQ_ALL_SUBSTANCES )

end function GetTotalSubstancesCount

!> Get the name of a substance
integer function GetSubstanceName( subid, name )

    !DEC$ ATTRIBUTES DLLEXPORT::GetSubstanceName
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETSUBSTANCENAME' :: GetSubstanceName

    use waq_omi_priv

    implicit none

    integer, intent(in)           :: subid
    character(len=*), intent(out) :: name

    GetSubstanceName = GetItemNamePriv( DLWQ_CONCENTRATION, subid, name )

end function GetSubstanceName

!> Get the index of a substance
integer function GetSubstanceId( name )

    !DEC$ ATTRIBUTES DLLEXPORT::GetSubstanceId
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETSUBSTANCEID' :: GetSubstanceId

    use waq_omi_priv

    implicit none

    character(len=*), intent(in)  :: name

    GetSubstanceId = GetItemIndexPriv( DLWQ_CONCENTRATION, name )

end function GetSubstanceId

!> Get the time period for the complete computation
integer function GetTimeHorizon( startTime, stopTime )

    !DEC$ ATTRIBUTES DLLEXPORT::GetTimeHorizon
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETTIMEHORIZON' :: GetTimeHorizon

    use waq_omi_priv

    implicit none

    real(kind=kind(1.0d0)), intent(out) :: startTime
    real(kind=kind(1.0d0)), intent(out) :: stopTime

    real(kind=kind(1.0d0))              :: timeStep
    real(kind=kind(1.0d0))              :: currentTime

    call GetTimeParameters( startTime, stopTime, timeStep, currentTime )

    GetTimeHorizon = 0

end function GetTimeHorizon

!> Get the current time for the computation
integer function GetWQCurrentTime( currentTime )

    !DEC$ ATTRIBUTES DLLEXPORT::GetWQCurrentTime
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETWQCURRENTTIME' :: GetWQCurrentTime

    use waq_omi_priv

    implicit none

    real(kind=kind(1.0d0)), intent(out) :: currentTime

    real(kind=kind(1.0d0))              :: startTime
    real(kind=kind(1.0d0))              :: stopTime
    real(kind=kind(1.0d0))              :: timeStep

    call GetTimeParameters( startTime, stopTime, timeStep, currentTime )
    GetWQCurrentTime = 0

end function GetWQCurrentTime

!> Get the next time in the computation
integer function GetWQNextTime( nextTime )

    !DEC$ ATTRIBUTES DLLEXPORT::GetWQNextTime
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETWQNEXTTIME' :: GetWQNextTime

    use waq_omi_priv

    implicit none

    real(kind=kind(1.0d0)), intent(out) :: nextTime

    real(kind=kind(1.0d0))              :: startTime
    real(kind=kind(1.0d0))              :: stopTime
    real(kind=kind(1.0d0))              :: timeStep

    call GetTimeParameters( startTime, stopTime, timeStep, nextTime )
    nextTime = nextTime + timeStep

    GetWQNextTime = 0

end function GetWQNextTime

