subroutine calpsh(restid    ,error     ,nmax      ,mmax      ,nmaxus    , &
                & kcs       ,pship     ,s1        ,gdp       )
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
!  $Id: calpsh.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/flow2d3d/packages/kernel/src/inichk/calpsh.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: - Computes pressure at floating structure
!                locations
!              - Check location of floating structure inside
!                computational domain
!              - Definition of all neighbour velocity points of
!                floating structure in KSPU/V for upwind alerady
!                in FLSFIL (RDSTRU)
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
    real(fp)               , pointer :: eps
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
!
! Global variables
!
    integer                                                      , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                      , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                      , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    logical                                                      , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: pship  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: s1     !  Description and declaration in esm_alloc_real.f90
    character(*)                                                 , intent(in)  :: restid !!  Runid of the restart file
!
! Local variables
!
    integer       :: lundia
    integer       :: m      ! Loop counter MMAX 
    integer       :: n      ! Loop counter NMAXUS 
    real(fp)      :: floini ! Flag for re-definition of S1 only if not a restart 
    character(16) :: errmsg ! Text containing position of floating structure 
!
!
!! executable statements -------------------------------------------------------
!
    eps       => gdp%gdconst%eps
    rhow      => gdp%gdphysco%rhow
    ag        => gdp%gdphysco%ag
    !
    floini = 1.0_fp
    if (restid /= ' ') then
       floini = 0.0_fp
    endif
    !
    ! Compute PSHIP (pressure generated by floating structure in N/m2)
    ! from PSHIP (input depth values)
    !
    do m = 1, mmax
       do n = 1, nmaxus
          if (pship(n,m) > eps) then
             write (errmsg, '(a8,2i4)') '(M,N) = ', m, n
             !
             ! Test discharge is inner active point (KCS = 1)
             !
             if (kcs(n, m) /= 1 .and. kcs(n, m) /= -1) then
                call prterr(lundia, 'V232', 'floating structure')
                write (lundia, '(20x,a)') errmsg
                error = .true.
                cycle
             endif
             !
             ! Re-define Waterlevel S1 value and PSHIP
             !
             s1   (n, m) = s1(n,m) - floini*pship(n,m)
             pship(n, m) = pship(n,m) * ag * rhow
          else
             !
             ! Avoid usage of negative values (-999.0)
             !
             pship(n,m) = 0.0_fp
          endif
       enddo
    enddo
end subroutine calpsh
