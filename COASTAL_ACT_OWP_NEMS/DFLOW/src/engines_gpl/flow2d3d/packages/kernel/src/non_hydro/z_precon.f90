subroutine z_precon(bbka      ,bbkc      ,pbbk      ,pbbkc     ,kmax      , &
                  & icx       ,icy       ,nmmax     ,kfsz0     ,rj        , &
                  & p1        ,kfs       ,kfsmin    ,kfsmx0    ,gdp       )
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
!  $Id: z_precon.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/flow2d3d/packages/kernel/src/non_hydro/z_precon.f90 $
!!--description-----------------------------------------------------------------
!
! Computes the preconditioning
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: m1_nhy
    integer , pointer :: m2_nhy
    integer , pointer :: n1_nhy
    integer , pointer :: n2_nhy
!
! Global variables
!
    integer                                         , intent(in) :: icx
    integer                                         , intent(in) :: icy
    integer                                         , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                      :: nmmax  !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: kfsz0  !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: bbka
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: bbkc
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: p1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: pbbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: pbbkc
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: rj
!
! Local variables
!
    integer :: ddb
    integer :: icxy
    integer :: k
    integer :: mink
    integer :: m
    integer :: ndelta
    integer :: nm
    integer :: nmst
    integer :: nmstart
!
!! executable statements -------------------------------------------------------
!
    m1_nhy  => gdp%gdnonhyd%m1_nhy
    m2_nhy  => gdp%gdnonhyd%m2_nhy
    n1_nhy  => gdp%gdnonhyd%n1_nhy
    n2_nhy  => gdp%gdnonhyd%n2_nhy
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    ndelta  = n2_nhy - n1_nhy
    nmstart = (n1_nhy + ddb) + (m1_nhy - 1 + ddb)*icxy
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          !
          ! in case of no preconditioning:
          !  do k = 1, kmax
          !     if (kfsz1(nm, k)/=0) then
          !        p1(nm, k) = rj(nm, k)
          !     endif
          !  enddo
          !
          !
          ! in case of ADI preconditioning in vertical direction:
          !
          if (kfs(nm) == 1) then
             mink = kfsmin(nm)
             if (kfsz0(nm,mink) /= 0) then
                p1(nm,mink) = rj(nm,mink)
             endif
             do k = mink+1, kfsmx0(nm)
                if (kfsz0(nm,k) /= 0) then
                   p1(nm,k) = ( rj(nm,k) - bbka(nm,k) * p1(nm,k-1) ) * pbbk(nm,k) 
                endif
             enddo
             do k = kfsmx0(nm)-1, mink, -1
                if (kfsz0(nm,k) /= 0) then
                   p1(nm,k) = p1(nm,k) - pbbkc(nm,k) * p1(nm,k+1)
                endif
             enddo
          endif
       enddo
    enddo
end subroutine z_precon
