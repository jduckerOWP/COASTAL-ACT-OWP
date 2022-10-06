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

! $Id: m_filter.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_data/m_filter.f90 $


module m_filter
   use m_solver
   
   type(tsolver) :: solver_filter
 
   double precision, dimension(:), allocatable :: ALvec2  !< vector Laplacian in CRS format, matrix entries
   
   integer,          dimension(:), allocatable :: iLvec  !< vector Laplacian in CRS format, startpointers
   integer,          dimension(:), allocatable :: jLvec  !< vector Laplacian in CRS format, row numbers
   double precision, dimension(:), allocatable :: ALvec  !< vector Laplacian in CRS format, matrix entries

   double precision, dimension(:), allocatable :: sol      !< solution of "filter" solve, dim(Lnx)
   double precision, dimension(:), allocatable :: ustar    !< predictor, dim(Lnkx)
   double precision, dimension(:,:), allocatable :: eps       !< filter coefficient, dim(kmx,Lnx)
   double precision, dimension(:), allocatable :: dtmaxeps  !< maximum time step multiplied with filter coefficient, dim(Lnx)
   double precision, dimension(:), allocatable :: Deltax    !< typical mesh width, dim(Lnx)
   double precision, dimension(:), allocatable :: checkmonitor   !< "checkerboard" mode monitor, dim(kmx+1)
   
   double precision, dimension(:), allocatable :: workin      !< work array, dim(kmx+1)
   double precision, dimension(:), allocatable :: workout     !< work array, dim(kmx+1)
   
   integer                                     :: order    !< order, 1st (1) or 3rd (3)
   integer                                     :: itype    !< explicit (1), implicit (2), implicit with hor. terms (3), no filter (0)
                     
   integer                                     :: jadebug = 0
   integer,          parameter                 :: LENFILNAM = 128
   character(len=LENFILNAM)                    :: FNAM      
   integer,          dimension(:), allocatable :: num
   integer,          dimension(:), allocatable :: dum
end module m_filter