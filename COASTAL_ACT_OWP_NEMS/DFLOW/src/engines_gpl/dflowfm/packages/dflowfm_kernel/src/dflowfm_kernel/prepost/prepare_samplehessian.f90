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

! $Id: prepare_samplehessian.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/prepare_samplehessian.f90 $

!> prepare the sample Hessians
subroutine prepare_sampleHessian(ierror)
   use m_samples
   use m_samples_refine

   implicit none

   integer, intent(out) :: ierror   !< error (1) or not (0)
   integer              :: jacancelled

   ierror = 1

   if ( iHesstat.ne.iHesstat_OK ) then
!     (re)allocate
      call allocate_sampleHessian()

!     copy and possibly smooth sample data to zss(1,:,:)
      call smooth_samples(MXSAM, MYSAM, NS, NDIM, Nsamplesmooth, zs, zss)
      Nsamplesmooth_last = Nsamplesmooth

!     compute sample Hessians
      call comp_sampleHessian(ierror)
      if ( ierror.ne.0 ) goto 1234
   end if

   iHesstat = iHesstat_OK

   ierror = 0
1234 continue

   return
end subroutine prepare_sampleHessian
