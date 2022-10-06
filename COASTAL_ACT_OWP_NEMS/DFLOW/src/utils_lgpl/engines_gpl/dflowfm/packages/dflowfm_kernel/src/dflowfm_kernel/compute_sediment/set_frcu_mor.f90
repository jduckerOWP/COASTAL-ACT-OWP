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

! $Id: set_frcu_mor.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_sediment/set_frcu_mor.f90 $

subroutine set_frcu_mor(dim)
    use m_flow, only: frcu, frcu_mor
    use m_flowgeom, only: lnx, lnxi, lnx1d, lnx1Db
    integer, intent(in) :: dim

    integer :: L

    if (dim == 1) then
       do L = 1, lnx1d  ! 1D int
           frcu_mor(L) = frcu(L)
       enddo
       do L = lnxi+1, lnx1Db ! 1D bnd
           frcu_mor(L) = frcu(L)
       enddo
    endif
    if (dim == 2) then
       do L = lnx1d+1, lnxi ! 2D int
           frcu_mor(L) = frcu(L)
       enddo
       do L = lnx1Db+1, lnx ! 2D bnd
           frcu_mor(L) = frcu(L)
       enddo
    endif
end subroutine set_frcu_mor
