function reddic (dicww, gdp)
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
!  $Id: reddic.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/flow2d3d/packages/kernel/src/compute/reddic.f90 $
!!--description-----------------------------------------------------------------
!
! Default: 
!    dicww = max (dicww , dicoww)
!
! Low Reynolds damping switched on:
!    dicww = f(damp) * dicww
!
!!--pseudo code and references--------------------------------------------------
!
! zheng.wang@deltares.nl
!
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
!
! Return value
!
real(fp) :: reddic
!
! Global variables
!
real(fp), intent(in)  :: dicww
!
! Local variables
!
real(fp) :: arg
!
!! executable statements -------------------------------------------------------
!
if (gdp%gdprocs%lrdamp) then
   arg = gdp%gdturcoe%lrdamp_fac * dicww
   if (arg < 3.0_fp) then
      reddic = (1.0_fp - exp(-arg*arg)) * dicww
   else
      reddic = max(dicww , gdp%gdphysco%dicoww)
   endif
else
   reddic = max(dicww , gdp%gdphysco%dicoww)
endif
end function reddic
