!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2022.                                
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
!  $Id: ftnunit_hooks.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/utils_lgpl/ftnunit/packages/ftnunit/src/ftnunit_hooks.f90 $
! ftnunit_hooks.f90 --
!     Auxiliary module that provides user-defineable methods
!
!     Purpose:
!     Make it easier to customise the behaviour of ftnunit.
!     For instance to interact with external programs
!
!     The reason for introducing this is the integration
!     with an automatic build system. This requires
!     the application to write messages with a certain
!     structure.
!     Another reason was to enable the program being
!     tested to start an Internet browser so that
!     the test report is readily viewable.
!
!     Note:
!     This version contains a dummy implementation only
!
module ftnunit_hooks
    implicit none

contains

! ftnunit_hook_test_start --
!     Called when the test starts
!
! Arguments:
!     text            Description of the test
!
subroutine ftnunit_hook_test_start( text )

    character(len=*) :: text

end subroutine ftnunit_hook_test_start

! ftnunit_hook_test_stop --
!     Called when the test stops (successful or not)
!
! Arguments:
!     text            Description of the test
!
subroutine ftnunit_hook_test_stop( text )

    character(len=*) :: text

end subroutine ftnunit_hook_test_stop

! ftnunit_hook_test_assertion_failed --
!     Called when an asserion failed
!
! Arguments:
!     text            Description of the test
!     assert_text     Description of the assertion
!     failure_text    Details of the failure
!
subroutine ftnunit_hook_test_assertion_failed( text, assert_text, failure_text )

    character(len=*) :: text
    character(len=*) :: assert_text
    character(len=*) :: failure_text

end subroutine ftnunit_hook_test_assertion_failed

! ftnunit_hook_test_completed --
!     Called when the tests are completed
!
! Arguments:
!     None
!
subroutine ftnunit_hook_test_completed

end subroutine ftnunit_hook_test_completed

end module ftnunit_hooks
