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

! $Id: update_vertadmin.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/update_vertadmin.f90 $

!> update the z-layer administration in parallel computations
!>    check bl and kmxn
!>    update kmxn, kmxL
subroutine update_vertadmin()
   use m_partitioninfo
   use m_flowgeom
   use m_flow
   use unstruc_messages
   implicit none

   character(len=128)                            :: mesg

   double precision, dimension(:,:), allocatable :: dum

   integer                                       :: k, L
   integer                                       :: n3, n4
   integer                                       :: ierror

   ierror = 0

   if ( jampi.eq.0 ) return   ! intended for parallel computations only

   if ( kmx.le.0 ) return    ! 3D only

!  allocate dummy array
   allocate(dum(2,Ndx))

!  bl, kmxn: filly dummy array
   do k=1,Ndx
      dum(1,k) = dble(kmxn(k))
      dum(2,k) = bl(k)
   end do
!  udpate dummy array
   call update_ghosts(ITYPE_SALL, 2, Ndx, dum, ierror)
   call update_ghostboundvals(ITYPE_SALL, 2, Ndx, dum, 0, ierror)
   if ( ierror.ne.0 ) then
      call mess(LEVEL_INFO, 'update_vertadmin: error')
      ierror = 1
      goto 1234
   end if

!  check bl and kmxn values
   do k=1,Ndx
      if ( int(dum(1,k)).ne.kmxn(k) ) then
         write(mesg,"('update_vertadmin: kmxn error, k=', I7 )") k
         call mess(LEVEL_INFO, mesg)
         ierror = 1
         goto 1234
      end if

      if ( dum(2,k).ne.bl(k) ) then
         write(mesg,"('update_vertadmin: bl error, k=', I7 )") k
         call mess(LEVEL_INFO, mesg)
         ierror = 1
         goto 1234
      end if
   end do

   if ( allocated(dum) ) deallocate(dum)
   allocate(dum(1,Lnx))

!  kmxL: filly dummy array
   do L=1,Lnx
      dum(1,L) = dble(kmxL(L))
   end do
!  update dummy array
   call update_ghosts(ITYPE_U, 1, Lnx, dum, ierror)

   if ( ierror.ne.0 ) then
      call mess(LEVEL_INFO, 'update_vertadmin: error')
      ierror = 1
      goto 1234
   end if

!  update values
   do L=1,Lnx
      if ( int(abs(dum(1,L))).ne.kmxL(L) ) then
!         write(mesg,"('update_vertadmin: kmxL error, L=', I7, 2I7 )") L, kmxL(L), int(dum(1,L))
!         call mess(LEVEL_INFO, trim(mesg))
!         ierror = 1

         kmxL(L) = int(abs(dum(1,L)))
      end if
   end do

!   if ( allocated(dum) ) deallocate(dum)
!   allocate(dum(2,Lnx))
!
!!  kmxc: fill dummy array
!   do L=1,Lnx
!      n3 = lncn(1,L)
!      n4 = lncn(2,L)
!      dum(1,L) = dble(kmxc(n3))
!      dum(2,L) = dble(kmxc(n4))
!   end do
!!  update dummy array
!   call update_ghosts(ITYPE_U, 2, Lnx, dum, ierror)
!
!   if ( ierror.ne.0 ) then
!      call mess(LEVEL_INFO, 'update_vertadmin: error')
!      ierror = 1
!      goto 1234
!   end if
!
!!  udpate values
!   do L=1,Lnx
!      n3 = lncn(1,L)
!      n4 = lncn(2,L)
!      if ( (int(dum(1,L)).ne.kmxc(n3) .or. int(dum(2,L)).ne.kmxc(n4)) ) then
!!         write(mesg,"('update_vertadmin: kmxc error, L=', I7 )") L
!!         call mess(LEVEL_INFO, trim(mesg))
!!         ierror = 1
!         kmxc(n3) = int(dum(1,L))
!         kmxc(n4) = int(dum(2,L))
!      end if
!   end do

   if ( ierror.eq.1 ) then
     call mess(LEVEL_ERROR, 'update_vertadmin: vertical layer administration out of sync', ' ', ' ')
!     stop
   end if

1234 continue
   if ( allocated(dum) ) deallocate(dum)

   return
end subroutine update_vertadmin
