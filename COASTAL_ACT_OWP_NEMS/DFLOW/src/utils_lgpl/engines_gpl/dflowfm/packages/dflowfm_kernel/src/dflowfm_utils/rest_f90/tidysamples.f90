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

! $Id: tidysamples.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/rest_f90/tidysamples.f90 $

      SUBROUTINE TIDYSAMPLES(XS,YS,ZS,IPSAM,NS,MXSAM,MYSAM)
      use sorting_algorithms, only: indexx
      implicit none
      integer :: ns
      double precision :: XS(NS), YS(NS), ZS(NS)   !< sample coordinates
      integer, dimension(NS), intent(out) :: IPSAM !< permutation array (increasing x-coordinate)
      integer,                intent(in)  :: MXSAM, MYSAM   !< structured sample data dimensions (>0) or unstructured (0)
!      IF (NS .GT. 1) CALL RSORT3(XS,YS,ZS,NS)

      if ( NS.gt.1 ) then
         call indexx(Ns,xs,IPSAM)
      end if

!     remove double/missing samples (non-structured sample data only)
      if ( MXSAM*MYSAM.ne.NS ) then
         CALL READYY(' ',0.3d0)
         IF (NS .GT. 1) CALL RMDOUBLE(XS,YS,ZS,IPSAM,NS)
      end if

      CALL READYY(' ',1d0)

      RETURN
      END
