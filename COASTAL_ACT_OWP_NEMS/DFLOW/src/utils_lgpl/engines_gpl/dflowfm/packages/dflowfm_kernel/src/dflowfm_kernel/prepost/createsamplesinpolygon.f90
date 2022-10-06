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

! $Id: createsamplesinpolygon.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/createsamplesinpolygon.f90 $

   SUBROUTINE CREATESAMPLESINPOLYGON()
   use m_polygon
   use m_missing
   use m_samples
   use geometry_module, only: get_startend   ! zijn er nog meer startends zodat dit afgeschermd moet worden?

   integer :: jpoint, jstart,jend,jadoall, nplsav
   double precision, allocatable :: xplsav(:), yplsav(:)

   allocate(xplsav(npl) , yplsav(npl)) ; xplsav = xpl(1:npl) ; yplsav = ypl(1:npl) ; nplsav = npl

   jpoint = 1; jadoall = 0
   do while ( jpoint.lt.NPLsav )

      !get subpolyline
      call get_startend(NPLsav-jpoint+1,xplsav(jpoint:NPLsav),yplsav(jpoint:NPLsav),jstart,jend, dmiss)
      xpl(1:jend-jstart+1) = xplsav(jstart+jpoint-1:jend+jpoint-1)
      ypl(1:jend-jstart+1) = yplsav(jstart+jpoint-1:jend+jpoint-1)
      npl = jend-jstart+1

      if (nplsav > jend) then
         jadoall = 1
      endif

      jstart = jstart+jpoint-1
      jend   = jend+jpoint-1
      jpoint = jend+2

      call CREATESAMPLESINPOLYGON2()

      if (jadoall == 1) then
         call Triangulatesamplestonetwork(1)
      endif

   enddo

   deallocate (xplsav, yplsav)

   END SUBROUTINE CREATESAMPLESINPOLYGON
