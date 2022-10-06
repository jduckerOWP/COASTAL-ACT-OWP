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

! $Id: m_profiles.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_data/m_profiles.f90 $

module m_profiles
 ! profile related :
 type tprof                                          !< this is a profile type
   integer                        :: ityp            !< 1 = circle, 2=rectan1dlumped, 3=rectan2d, 9=yzlumped, 10=yzconveyance
   integer                        :: frctp           !< friction type chezy manning etc
   double precision               :: frccf           !< friction coefficient
   double precision               :: width           !< max width
   double precision               :: height          !< max height
   real, allocatable              :: xx(:), yy(:)    !< y z point coordinates
   real, allocatable              :: y (:), z (:)    !< y and z arrays
   real                           :: zmin            ! absolute z lavel of lowest point
 end type tprof

 integer                          :: nprofdefs       !< nr of unique  profile definitions
 type(tprof), allocatable         :: profiles1D(:)   !< these are the profiles

 integer                          :: nproflocs = 0      !< nr of profile locations, always <= nprofdefs
 integer                          :: maxproflocnr       !< highest referred profnr in profloc.xyz
 integer                          :: minproflocnr       !< lowest  referred profnr in profloc.xyz
 double precision, allocatable    :: xpr(:), ypr(:), zpr(:) !< profile locations, x,y,z
 integer,          allocatable    :: npr(:)                 !< at these locations, reference to profdefs
 integer                          :: jainterpolatezk1D = 1
 double precision                 :: tolzprof = 0.1d0
 integer                          :: ntolsave = 0
end module m_profiles
