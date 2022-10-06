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

! $Id: wrirst.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_io/wrirst.f90 $

subroutine wrirst(tim)
    use m_flow
    use m_flowtimes
    use m_observations
    use unstruc_netcdf
    use unstruc_model
    use unstruc_files , only: defaultFilename
    implicit none
    double precision, intent(in) :: tim

    ! locals
    integer, save      :: irstfile = 0
    integer            :: ierr
    character(len=256) :: filnam

    if (irstfile == 0) then
        filnam = defaultFilename('rst', timestamp=tim ) ! dble(floor(tim+.5d0)))
        ierr   = unc_create(filnam , 0, irstfile)
        if (ierr /= nf90_noerr) then
            call mess(LEVEL_WARN, 'Could not create rst file.')
            irstfile = 0
        end if
    endif

    if (irstfile .ne. 0) then
        call unc_write_rst_filepointer(irstfile,tim)
    endif

    ierr = unc_close(irstfile) ! Do more than flushing: close the file, it is not needed anymore

 end subroutine wrirst
