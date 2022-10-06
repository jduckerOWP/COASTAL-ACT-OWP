subroutine mirror_bnd(icx       ,icy       ,nmmax     , &
                    & kcs       ,fld       ,nmlb      ,nmub      )
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
!  $Id: mirror_bnd.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/flow2d3d/packages/kernel/src/inichk/mirror_bnd.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: - Initialisation total sediment at bed in each
!                horizontal point
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
!
! Global variables
!
    integer                                              , intent(in)   :: icx
    integer                                              , intent(in)   :: icy
    integer                                              , intent(in)   :: nmlb
    integer                                              , intent(in)   :: nmub
    integer                                              , intent(in)   :: nmmax  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(nmlb:nmub)                     , intent(in)   :: kcs    !  Description and declaration in esm_alloc_int.f90
    real(fp)  , dimension(nmlb:nmub)                     , intent(inout):: fld
!
! Local variables
!
    integer           :: nm
    integer           :: nm2
!
!! executable statements -------------------------------------------------------
!
    do nm = 1,nmmax
       if (kcs(nm)==2 .and. fld(nm)<0.0) then
          if (kcs(nm-icx) == 1) then
             ! ndm
             nm2 = nm-icx
          elseif (kcs(nm+icx) == 1) then
             ! num
             nm2 = nm+icx
          elseif (kcs(nm-icy) == 1) then
             ! nmd
             nm2 = nm-icy
          else
             ! nmu
             nm2 = nm+icy
          endif
          fld(nm) = fld(nm2)
       endif
    enddo
end subroutine mirror_bnd
