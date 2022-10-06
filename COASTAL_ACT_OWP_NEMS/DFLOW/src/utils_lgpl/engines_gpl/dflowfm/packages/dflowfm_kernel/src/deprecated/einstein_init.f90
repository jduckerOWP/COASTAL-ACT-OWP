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

! $Id: einstein_init.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/deprecated/einstein_init.f90 $

subroutine einstein_init()
use m_einstein_garcia
implicit none

c1(1,:)  = (/    8.03210,  -26.27300, -114.69000,  501.43000, -229.51000,   41.94000,   -2.77220  /)
c1(2,:)  = (/    2.11420,   -3.45020,   12.49100,   60.34500,  -29.42100,    5.42150,   -0.35770  /)
c1(3,:)  = (/    1.48520,    0.20250,   14.08700,   20.91800,  -10.91000,    2.03400,   -0.13450  /)
c1(4,:)  = (/    1.10380,    2.66260,    5.64970,    0.38220,   -0.61740,    0.13150,   -0.00910  /)
c1(5,:)  = (/    1.12660,    2.62390,    3.08380,   -0.36360,   -0.07340,    0.02460,   -0.00190  /)

c2(1,:)  = (/    2.57790,  -12.41800,   47.35300,   17.63900,  -13.55400,    2.83920,   -0.20030  /)
c2(2,:)  = (/    1.26230,    1.03300,   13.54300,    0.76550,   -1.66460,    0.38030,   -0.02750  /)
c2(3,:)  = (/    1.15100,    2.17870,    7.65720,   -0.27770,   -0.57000,    0.14240,   -0.01050  /)
c2(4,:)  = (/    1.25740,    2.31590,    1.92390,   -0.35580,    0.00750,    0.00640,   -0.00060  /)
c2(5,:)  = (/    1.49520,    2.20410,    1.05520,   -0.23720,    0.02650,   -0.00080,   -0.00005  /)

d(1:5)   = (/    0.001d0,    0.005d0,     0.01d0,     0.05d0,      0.1d0                          /)

end subroutine einstein_init
