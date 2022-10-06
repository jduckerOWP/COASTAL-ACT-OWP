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

! $Id: dateandtimenow.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/rest_f90/dateandtimenow.f90 $

      subroutine dateandtimenow(iyear, month, iday, ihour, minute, isecnd)
      implicit none
      integer,            intent(out)              :: iyear, month, iday, ihour, minute, isecnd
!     integer,            intent(out), optional    :: imsec
!     character(len=5),   intent(out), optional    :: zone

      character(len=8 ) ::       dat
      character(len=10) ::       tim
      character(len=5)  ::       zone
      integer           ::       imsec
      integer           ::       values(8)

      call date_and_time(dat,tim,zone,values)
      iyear  = values(1)
      month  = values(2)
      iday   = values(3)
      ihour  = values(5)
      minute = values(6)
      isecnd = values(7)
      imsec  = values(8)
      end subroutine dateandtimenow
