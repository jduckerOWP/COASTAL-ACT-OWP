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

! $Id: alloc_jacobi.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_data/alloc_jacobi.f90 $

 subroutine alloc_jacobi(ndx,lnx)
 use m_jacobi
 use m_alloc
 integer :: ndx, lnx, ierr

 if (ndx == ndxjac .and. lnx == lnxjac) return

 if (allocated(bbi) ) then
    deallocate(bbi,db,rr)
 endif

 allocate ( bbi  (ndx) , stat = ierr)
 call aerr('bbi  (ndx)', ierr,   ndx) ; bbi = 0
 allocate ( db   (ndx) , stat = ierr)
 call aerr('db   (ndx)', ierr,   ndx) ; db  = 0
 allocate ( rr   (ndx) , stat=ierr )
 call aerr('rr   (ndx)', ierr, ndx )  ; rr  = 0

end subroutine alloc_jacobi
