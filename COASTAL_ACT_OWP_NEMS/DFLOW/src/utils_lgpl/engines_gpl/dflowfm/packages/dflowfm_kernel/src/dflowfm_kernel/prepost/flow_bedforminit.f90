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

! $Id: flow_bedforminit.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/flow_bedforminit.f90 $

!
! Bedform prediction routines
!
subroutine flow_bedforminit(stage)
   use m_bedform
   use m_bedform_io, only: fm_rdbedformpar, fm_initbedformpar
   use unstruc_model, only: md_bedformfile
   use MessageHandling, only: mess, LEVEL_FATAL

   implicit none

   logical                      :: error
   integer, intent(in)          :: stage

   if (stage==1) then

      call fm_initbedformpar(bfmpar, error)              ! need to initialize the data structure for
                                                         ! eg dredge, tauwave and bed roughness, even if no bedformfile there..
                                                         ! this resets bfmpar%lfbedfrmrou = .true. to .false., so need two stages:
                                                         ! one before sedmorinit, and one after
      if (error) then
         call mess(LEVEL_FATAL, 'unstruc::flow_bedforminit - Error in initialisation of bedform module.')
         return
      end if

   else if (stage==2) then

      !
      bfm_included = len_trim(md_bedformfile) /= 0
      if (.not. bfm_included) return
      !
      call fm_rdbedformpar(bfmpar, md_bedformfile, error)
      if (error) then
         call mess(LEVEL_FATAL, 'unstruc::flow_bedforminit - Error in reading of bedform file.')
         return
      end if
   end if

end subroutine flow_bedforminit
