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

! $Id: gettaus.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/gettaus.f90 $


! $Id: gettaus.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/gettaus.f90 $

subroutine gettaus(typout, kernel)
   use m_flowgeom
   use m_flow
   use m_alloc
   use m_flowparameters, only: flowWithoutWaves, jawaveswartdelwaq
   !
   implicit none
   !
   ! Parameters
   integer, intent (in)       ::  typout   !< type of setting, 1: set czs and taus, 2: just set czs:
   integer, intent (in)       ::  kernel   !< kernel requesting to compute taus, 1: D-Flow FM, 2: D-WAQ
   !
   ! Locals
   double precision           ::  taucurc  !< local variable for taucurrent
   double precision           ::  czc      !< local variable for chezy
   integer                    ::  ierr     !< Error code
   integer                    ::  n        !< Counter
   integer                    ::  jawaveswartdelwaq_local !< Local value of jawaveswartdelwaq, depending on kernel and flowWithoutWaves
   !
   ! Body
   if (flowWithoutWaves .and. kernel==1) then
      jawaveswartdelwaq_local = 0
   else
      jawaveswartdelwaq_local = jawaveswartdelwaq
   endif
   if (.not. allocated(czs) ) then
       call realloc(czs,  ndxi,  keepExisting = .false., fill = 0d0, stat = ierr)
   else if (size(czs) < ndxi) then
       call realloc(czs,  ndxi,  keepExisting = .false., fill = 0d0, stat = ierr)
   endif
   if (typout == 1) then
       if (.not. allocated(taus) ) then
           call realloc(taus,  ndxi,  keepExisting = .false., fill = 0d0, stat = ierr)
       else if (size(taus) < ndxi) then
           call realloc(taus,  ndxi,  keepExisting = .false., fill = 0d0, stat = ierr)
       endif
   endif

   do n = 1,ndxi
      call gettau(n,taucurc,czc,jawaveswartdelwaq_local)
      czs(n) = czc
      if (typout == 1) then
          taus(n) = taucurc
      endif
   enddo
end subroutine gettaus
