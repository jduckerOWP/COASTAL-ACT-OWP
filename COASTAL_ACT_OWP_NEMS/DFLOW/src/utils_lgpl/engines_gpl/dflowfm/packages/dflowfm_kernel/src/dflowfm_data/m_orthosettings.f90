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

! $Id: m_orthosettings.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_data/m_orthosettings.f90 $

!> Orthogonalisation settings, both for regular grids and unstructured nets.
module m_orthosettings
implicit none
integer          :: ITATP = 2   !< Nr. of outer    iterations in grid/net orthogonalisation.
integer          :: ITBND = 25  !< Nr. of boundary iterations in grid/net orthogonalisation. (within ITATP)
integer          :: ITIN  = 25  !< Nr. of inner    iterations in grid/net orthogonalisation. (within ITBND)
                                !! Also used within structured grid orthogonalisation.
integer          :: JAPROJECT = 1   !< Project nodes back to boundary (2: yes, all, 1:yes, net bounds only, 0:no)
double precision :: ATPF = 0.975d0  !< Factor (0.<=ATPF<=1.) between grid smoothing and grid ortho resp.
double precision :: ATPF_B = 1d0 !< minimum ATPF on the boundary
double precision :: circumormasscenter = 1d0          !< 1.0 = circumcentre,      0.0 = masscentre, 1.0 -> 0.0 : weighted
double precision :: smoothorarea    = 1d0   !< Factor between smoother (1d0) and area-homogenizer (0d0)
integer          :: adapt_method    = 1     !< Mesh-adaptation method; 0: Winslow, 1: arc-length, 2: harmonic map
double precision :: adapt_beta      = 0.0d0 !< Mesh-refinement factor; between 0d0 and 1d0
integer          :: adapt_niter_u   = 0     !< number of smoothing iterations of `solution` u in adaptation
integer          :: adapt_niter_G   = 4     !< number of smoothing iterations of monitor matrix G in adaptation
double precision :: ortho_pure      = 0.5d0   !< curvi-linear-like (0d0) or pure (1d0) orthogonalisation

end module m_orthosettings
