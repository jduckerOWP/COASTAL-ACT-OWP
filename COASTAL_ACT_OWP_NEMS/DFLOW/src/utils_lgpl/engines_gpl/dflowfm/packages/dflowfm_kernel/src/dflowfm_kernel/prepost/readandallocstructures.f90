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

! $Id: readandallocstructures.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/readandallocstructures.f90 $

subroutine readandallocstructures( )
use m_strucs
use m_alloc
implicit none
integer :: i, ierr

call realloc(strhis, mxstrhis, nstru)
call aerr('strhis(mxstrhis,nstru)' ,ierr , mxstrhis*nstru )

call realloc(strhis2, mxstrhis, nstru)
call aerr('strhis2(mxstrhis,nstru)' ,ierr , mxstrhis*nstru )

mxgeneral   = 0d0
mxuniversal = 0d0

do i = 1,nstru
   if (itypstr(i) == ST_GENERAL_ST) then
      mxgeneral   = mxgeneral + 1
      ntypstr(i)  = mxgeneral
   else if (itypstr(i) == ST_UNI_WEIR) then
      mxuniversal = mxuniversal + 1
      ntypstr(i)  = mxuniversal
   endif
enddo

if (mxgeneral > 0) then
   if (allocated (generalstruc) ) deallocate (generalstruc)
   allocate (generalstruc(mxgeneral), stat=ierr)
endif

if (mxuniversal > 0) then
   if (allocated (universalstruc) ) deallocate (universalstruc)
   allocate (universalstruc(mxuniversal), stat=ierr)
endif

end subroutine readandallocstructures
