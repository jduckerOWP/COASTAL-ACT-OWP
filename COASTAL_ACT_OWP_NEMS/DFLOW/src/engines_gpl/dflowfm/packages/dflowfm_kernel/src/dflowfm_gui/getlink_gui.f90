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

! $Id: getlink_gui.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/getlink_gui.f90 $

!> select link for directional refinement in GUI
   subroutine getlink_GUI(xp, yp, L)
      implicit none

      double precision, intent(out) :: xp, yp   !< coordinates of clicked point
      integer,          intent(out) :: L        !< clicked link number

      double precision     :: zp

      integer              :: num, nwhat, nput, numb, key

      L     = 0

      call ktext(' Refine net       ',1,3,15)

      num   = 0
      nwhat = 0
      nput  = 55
      numb  = 10
      key   = 0

      do
         CALL DRAWNU(KEY)
         call putget_un(num,nwhat,nput,numb,xp,yp,key)

         if ( key.eq.23 )  then     ! escape
            exit
         else if ( key.eq.21) then  ! left mouse button
            call islink(L,xp,yp,zp)
            if ( L.gt.0 ) then   ! link found
               call teklink(L,31)
               exit
            end if
!        the following is copied from editnetw (zoom, panning)
         else if (key .eq. 43 .or. key .eq. 140) then
            call kplotplusmin(1)
            key = 3
         else if (key .eq. 45 .or. key .eq. 141) then
            call kplotplusmin(-1)
            key = 3
         else if (key .eq. 133) then        ! page down
            call nplotplusmin(1)
            key = 3
         else if (key .eq. 143) then        ! delete
            call nplotplusmin(-1)
            key = 3
         end if
      end do

      if ( L.lt.1 ) then
         call qnerror('no link clicked: exitting', ' ', ' ')
      end if

      return
   end subroutine
