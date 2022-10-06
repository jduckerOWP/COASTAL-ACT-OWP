subroutine run_swan_slave (command, retval)
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
!  $Id: run_swan_slave.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/wave/packages/kernel/src/run_swan_slave.f90 $
!!--description-----------------------------------------------------------------
!
!     *** Run swan; produce output file swanout with values on     ***
!     *** swan computational grid                                  ***
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use wave_mpi, only: wave_mpi_bcast, wave_mpi_barrier, engine_comm_world, MPI_SUCCESS, SWAN_GO
   implicit none
!
! Global variables
!
   integer, intent(out) :: command
   integer, intent(out) :: retval
!
! Local variables
!
   integer :: ierr
!
!! executable statements -----------------------------------------------
!
   call wave_mpi_bcast(command, ierr)
   if ( ierr == MPI_SUCCESS ) then
      if (command == SWAN_GO) then
         call swan(engine_comm_world)
         ! include barrier to make sure that all SWAN instances have
         ! finished accessing files that the master WAVE thread owill read
         ! and delete
         call wave_mpi_barrier(ierr)
      endif
   endif
   if ( ierr /= MPI_SUCCESS ) then
      write (*,'(a,i5)') 'MPI produces some internal error - return code is ',ierr
      retval = -1
   else
      retval = 0
   endif
end subroutine run_swan_slave
