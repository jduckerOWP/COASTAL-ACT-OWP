subroutine dhkmrk ( iknmrk , kenmrk , knmrki )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2012-2022.                                
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
!  $Id: dhkmrk.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/utils_gpl/morphology/packages/morphology_proclib/src/dhkmrk.f90 $
!!--description-----------------------------------------------------------------
!
! Utility that evaluates a feature from the "feature" integer
!
!!--declarations----------------------------------------------------------------
    integer, intent(in)  :: iknmrk  ! Index of feature
    integer, intent(in)  :: kenmrk  ! Feature
    integer, intent(out) :: knmrki  ! Evaluated feature
!
!   Local
!
    integer dhimis
!
    if ( iknmrk .eq. 1 ) then
       knmrki = mod(kenmrk,10)
    elseif ( iknmrk .eq. 2 ) then
       knmrki = kenmrk / 10
       knmrki = mod(knmrki,10)
    elseif ( iknmrk .le. 0 .or. iknmrk .gt. 9 ) then
       dhimis = -999.
       knmrki = dhimis
    else
       knmrki = kenmrk / 10**(iknmrk-1)
       knmrki = mod(knmrki,10)
    endif

    return
end subroutine
