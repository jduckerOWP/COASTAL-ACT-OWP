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

! $Id: read_required_property.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/read_required_property.f90 $

!> Reads a key=value entry from a property block and tries to interpret the value.
!! The (single!) property block should come from an already-parsed .ini file.
!! The string value is always returned, if found, and an attempt is also made to
!! parse it into a scalar double, or alternatively to check whether it is an existing file.
subroutine read_required_property(prop_ptr, key, strvalue, dblvalue, is_double, typeandid, success)
   use properties
   use unstruc_messages
   implicit none
   type(TREE_DATA), pointer        :: prop_ptr   !< Property tree as read from a single .ini block
   character(len=*), intent(in)    :: key        !< Property key that should be read.
   character(len=*), intent(inout) :: strvalue   !< Returned string value for requested property key.
   double precision, intent(inout) :: dblvalue   !< Returned scalar double value for requested property key, IF possible.
   logical,          intent(out)   :: is_double  !< Tells whether the found value could be parsed into a scalar double value.
   character(len=*), intent(in)    :: typeandid  !< String with type and name, to be used in warning message to be printed if property key not found. Example: "gate 'Maeslant'"
   logical,          intent(out)   :: success    !< Whether value was read successfully or not.

   double precision :: tmpvalue
   integer :: ierr

   success   = .false.
   is_double = .false.

   call prop_get(prop_ptr, '', trim(key), strvalue, success)
   if (.not. success .or. len_trim(strvalue) == 0) then
      write(msgbuf, '(a,a,a,a,a)') 'Required field ''', trim(key), ''' missing in ', trim(typeandid), '.'
      call warn_flush()
      goto 888
   else
      read(strvalue, *, iostat = ierr) tmpvalue
      if (ierr == 0) then
         dblvalue = tmpvalue
         is_double = .true.
      end if
   end if

   success = .true.
888 continue

end subroutine read_required_property
