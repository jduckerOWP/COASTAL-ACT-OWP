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

! $Id: smooth_samples.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/smooth_samples.f90 $

!> smooth structured sample data and put it in zss(1,:,:)
!>    D u/ D t = div grad u
subroutine smooth_samples(MXSAM, MYSAM, NS, NDIM, Nsamplesmooth, zs, zss)
!   use m_samples
!   use m_samples_refine
   use m_missing

   implicit none

   integer,                                       intent(in)    :: MXSAM, MYSAM  !< structured block sizes (>0) or not structured (0)
   integer,                                       intent(in)    :: NS            !< number of samples
   integer,                                       intent(in)    :: NDIM          !< number of variable per sample in zss
   integer,                                       intent(in)    :: Nsamplesmooth !< number of smoothing iterations

   double precision, dimension(Ns),               intent(in)    :: zs            !< sample input variables, dim(NS)
   double precision, dimension(NDIM,MXSAM,MYSAM), intent(inout) :: zss           !< sample output variables, dim(NDIM,MXSAM,MYSAM), only first component will be smoothed

   double precision, dimension(:,:),              allocatable   :: zsdum

   integer                                                      :: iter, i, j
   integer                                                      :: ip0, ipiL, ipiR, ipjL, ipjR
   double precision                                             :: c0, ciL, ciR, cjL, cjR, af

   integer                                                      :: ierror

   double precision,                              parameter     :: sigma = 0.5d0

   ierror = 1

!  check if samples are structured
   if ( MXSAM*MYSAM.ne.NS ) goto 1234

!  allocate
   allocate(zsdum(MXSAM,MYSAM))

!  initialize zss(1,:,:)
   do i=1,MXSAM
      do j=1,MYSAM
         zss(1,i,j) = zs(i+MXSAM*(j-1))
      end do
   end do

   call readyy('Smoothing samples', 0d0)

!  Elliptic smoothing
   do iter=1,Nsamplesmooth
      af = dble(iter-1)/dble(max(Nsamplesmooth-1,1))
      call readyy('Smoothing samples', af)

!     copy zss(1,:,:) to zsdum
      do j=1,MYSAM
         do i=1,MXSAM
            zsdum(i,j) = zss(1,i,j)
         end do
      end do

      do j=2,MYSAM-1       ! inner nodes only
         do i=2,MXSAM-1    ! inner nodes only
            if ( zsdum(i,j).eq.DMISS ) cycle

!           compute weights
            ciL = 1d0
            ciR = 1d0
            cjL = 1d0
            cjR = 1d0
            if ( zsdum(i-1,j).eq.DMISS ) ciL = 0d0
            if ( zsdum(i+1,j).eq.DMISS ) ciR = 0d0
            if ( zsdum(i,j-1).eq.DMISS ) cjL = 0d0
            if ( zsdum(i,j+1).eq.DMISS ) cjR = 0d0

            if ( ciL*ciR*cjL*cjR.eq.0d0 ) cycle ! inner samples only

            c0 = ciL+ciR+cjL+cjR
            if ( abs(c0).lt.0.5d0 ) cycle

            zss(1,i,j) = (1d0-sigma) * zsdum(i,j) +   &
                              sigma  * (              &
                                          ciL*zsdum(i-1,j) +   &
                                          ciR*zsdum(i+1,j) +   &
                                          cjL*zsdum(i,j-1) +   &
                                          cjR*zsdum(i,j+1)     &
                                          ) / c0
         end do
      end do
   end do

   ierror = 0
!   Nsamplesmooth_last = Nsamplesmooth

   call readyy('Smoothing samples', -1d0)

1234 continue

!  deallocate
   if ( allocated(zsdum) ) deallocate(zsdum)

   return
end subroutine smooth_samples
