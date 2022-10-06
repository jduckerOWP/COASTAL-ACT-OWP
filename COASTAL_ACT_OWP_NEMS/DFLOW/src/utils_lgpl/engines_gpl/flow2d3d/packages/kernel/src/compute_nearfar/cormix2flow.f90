subroutine cormix2flow(thick  ,kmax  ,dps   ,s1    ,disch_nf ,sour_nf , &
                     & lstsci ,lsal  ,ltem  ,xz    ,yz       ,nmmax   , &
                     & kcs    ,gdp   )
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
!  $Id: cormix2flow.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/cormix2flow.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Converts cormix output to delft3d sources
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer           , pointer :: m_diff
    integer           , pointer :: n_diff
    real(fp)          , pointer :: q_diff
    real(fp)          , pointer :: t0_diff
    real(fp)          , pointer :: s0_diff
!
! Global variables
!
    integer                                                    , intent(in)  :: kmax     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)  :: lstsci   !  Description and declaration in tricom.igs
    integer                                                    , intent(in)  :: lsal     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)  :: ltem     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)  :: nmmax    !  Description and declaration in tricom.igs
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs      !  Description and declaration in
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s1       !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: xz       !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: yz       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: disch_nf !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax,lstsci) , intent(out) :: sour_nf  !  Description and declaration in
    real(fp)   , dimension(kmax)                               , intent(in)  :: thick    !  Description and declaration in esm_alloc_real.f90 gs
    real(prec) , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps      !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                                     :: nm_diff
    integer                      , external     :: newlun
    integer                                     :: luntmp
    integer                                     :: iocond
    integer                                     :: irow
    integer                                     :: itel
    integer                                     :: nrow
    real(fp)                                    :: xx
    real(fp)                                    :: yy
    real(fp)                                    :: zz
    real(fp)                                    :: ss
    real(fp)      , dimension(:) , allocatable  :: x_jet
    real(fp)      , dimension(:) , allocatable  :: y_jet
    real(fp)      , dimension(:) , allocatable  :: z_jet
    real(fp)      , dimension(:) , allocatable  :: s_jet
    character(80)                               :: record
!
!! executable statements -------------------------------------------------------
!
    m_diff         => gdp%gdnfl%m_diff
    n_diff         => gdp%gdnfl%n_diff
    q_diff         => gdp%gdnfl%q_diff
    t0_diff        => gdp%gdnfl%t0_diff
    s0_diff        => gdp%gdnfl%s0_diff
    !
    allocate (x_jet(1000))
    allocate (y_jet(1000))
    allocate (z_jet(1000))
    allocate (s_jet(1000))
    !
    call n_and_m_to_nm(n_diff, m_diff, nm_diff, gdp)
    !
    ! Open cormix output file and read jet characteristics end of near field
    ! end of corjet computation
    !
    open (newunit=luntmp,file='corjet.out',status='old')
    record (1:13) = '             '
    !
    do while (record (6:13) .ne. 'X      Y')
       read (luntmp,'(a80)') record
    enddo
    itel = 1
    read (record(1:47),*,iostat = iocond) x_jet(itel), y_jet(itel), z_jet(itel), s_jet(itel)
    !
    do while (record(2:13) .ne. '------------')
       read (luntmp,'(a80)') record
       read (record(1:47),*,iostat = iocond) xx, yy, zz, ss
       if (iocond == 0) then
          itel = itel + 1
          if (itel == 1000) stop 'Enlarge allocatable arrays in cormix2flow'
          x_jet(itel) = xx
          y_jet(itel) = yy
          z_jet(itel) = zz
          s_jet(itel) = ss
       endif
    enddo
    !
    nrow = itel - 1
    !
    ! Determine angle jet end near field
    !
    do irow = 1, nrow
       x_jet(irow) = xz(nm_diff) + x_jet(irow)
       y_jet(irow) = yz(nm_diff) + y_jet(irow)
       z_jet(irow) = real(dps(nm_diff),fp) - z_jet(irow)
    enddo
    !
    ! Fill sources and sinks following the Desa Method of Prof. Lee
    !
    call desa(x_jet   ,y_jet    ,z_jet   ,s_jet   ,nrow    , &
            & kcs     ,xz       ,yz      ,dps     ,s1      , &
            & nmmax   ,thick    ,kmax    ,lstsci  ,lsal    , &
            & ltem    ,disch_nf ,sour_nf ,gdp     )
    !
    ! Deallocate temporary arrays
    !
    deallocate (x_jet)
    deallocate (y_jet)
    deallocate (z_jet)
    deallocate (s_jet)
    !
    close(luntmp)
end subroutine cormix2flow
