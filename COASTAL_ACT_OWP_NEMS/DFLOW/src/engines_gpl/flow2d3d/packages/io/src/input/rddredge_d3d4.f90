subroutine rddredge_d3d4(gsqs      ,gdp       )
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
!  $Id: rddredge_d3d4.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/flow2d3d/packages/io/src/input/rddredge_d3d4.f90 $
!!--description-----------------------------------------------------------------
!
! Reads Dredge and Dump input file.
! Allocates related arrays.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use m_rddredge, only: rddredge
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: gsqs    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    type(tree_data), pointer                :: dad_ptr
    logical                                 :: error
!
!! executable statements -------------------------------------------------------
!
    call tree_create_node( gdp%input_tree, 'Dredge and Dump', dad_ptr )
    call rddredge(gdp%gddredge, dad_ptr, gdp%gdsedpar, gdp%gdbedformpar%lfbedfrm, &
                & gdp%gdmorpar, gdp%gdinout%lundia, gdp%gdinttim%julday, &
                & gsqs, gdp%griddim, gdp%runid, gdp%d%nmlb, gdp%d%nmub, error)
end subroutine rddredge_d3d4
