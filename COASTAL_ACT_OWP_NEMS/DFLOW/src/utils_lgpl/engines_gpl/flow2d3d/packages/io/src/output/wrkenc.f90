subroutine wrkenc(comfil    ,lundia    ,error     ,mmax      ,nmax      , &
                & nmaxus    ,kcu       ,kcv       ,kcs       ,ibuff     , &
                & gdp       )
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
!  $Id: wrkenc.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/flow2d3d/packages/io/src/output/wrkenc.f90 $
!!--description-----------------------------------------------------------------
!
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use datagroups
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical                  , pointer :: first
    type (datagroup)         , pointer :: group
!
! Global variables
!
    integer                                                                   :: lundia !  Description and declaration in inout.igs
    integer                                                                   :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                   :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                   :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(nmaxus, mmax)                                          :: ibuff  !  Description and declaration in esm_alloc_int.f90
    logical                                                     , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    character(*)                                                              :: comfil !!  Name for communication file
                                                                                        !!  com-<case><label>
!
! Local variables
!
    integer                                       :: fds
    integer                                       :: ierror ! Flag for error when writing to Communication file 
    integer                                       :: m
    integer                                       :: n
    integer      , dimension(3,5)                 :: uindex
    integer                        , external     :: putelt
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    integer                        , external     :: neferr
    character(16)                                 :: grpnam ! Data-group name defined for the COM-files 
    character(256)                                :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
!
! Data statements
!
    data grpnam/'KENMCNST'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_COM, grpnam, group)
    first   => group%first
    !
    if (first) then
       !
       ! Set up the element chracteristics
       !
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'KCU', ' ', IO_INT4, 2, (/nmaxus, mmax/), ' ', '0/1 Non-active/Active u-point', '[   -   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'KCV', ' ', IO_INT4, 2, (/nmaxus, mmax/), ' ', '0/1 Non-active/Active v-point', '[   -   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'KCS', ' ', IO_INT4, 2, (/nmaxus, mmax/), ' ', '0/1/2 Non-active/Active/Boundary water level point', '[   -   ]')
    endif
    !
    ierror = open_datdef(comfil   ,fds      , .false.)
    if (ierror /= 0) goto 9999
    !
    if (first) then
       call defnewgrp(fds, FILOUT_COM, grpnam, gdp, comfil, errlog=ERRLOG_NONE)
       first = .false.
    endif
    !
    ! initialize group index
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    ! element 'KCU'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          ibuff(n, m) = kcu(n, m)
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'KCU', uindex, 1, ibuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'KCV'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          ibuff(n, m) = kcv(n, m)
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'KCV', uindex, 1, ibuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'KCS'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          ibuff(n, m) = kcs(n, m)
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'KCS', uindex, 1, ibuff)
    if (ierror/= 0) goto 9999
    !
    ierror = clsnef(fds)
    !
    ! write error message if error occured and set error= .true.
    !
9999   continue
    if (ierror /= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error= .true.
    endif
end subroutine wrkenc
