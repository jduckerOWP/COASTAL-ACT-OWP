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

! $Id: delete_dry_points_and_areas.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/delete_dry_points_and_areas.f90 $

 ! Delete dry points from netgeom based on drypoints files and grid enclosure file
 subroutine delete_dry_points_and_areas()
   use unstruc_model, only: md_dryptsfile, md_encfile
   use gridoperations, only: update_cell_circumcenters
   implicit none

   call delete_drypoints_from_netgeom(md_dryptsfile, 0, 0)
   call delete_drypoints_from_netgeom(md_encfile, 0, -1)
!   call delete_drypoints_from_netgeom(md_cutcelllist, 0, 0)

   ! for issue UNST-3381, compute circumcenter after deleting dry areas
   ! TODO: UNST-3436 must be done as a better solution
   if (len_trim(md_dryptsfile) > 0 .or. len_trim(md_encfile) > 0) then
      call update_cell_circumcenters()
   end if

   return
 end subroutine delete_dry_points_and_areas
