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

! $Id: timestepanalysis.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/timestepanalysis.f90 $

 subroutine timestepanalysis(dtsc_loc)
    use m_flow
    use m_flowtimes
    use m_partitioninfo
    use unstruc_model, only: md_ident
    implicit none

    double precision, intent(in) :: dtsc_loc

    integer,          save       :: mout = 0

!   check if local maximum time step is also global maximum time step
    if ( jampi.eq.1 ) then
       if ( dtsc_loc.gt.dtsc ) then
          kkcflmx = 0
       end if
    end if

    if (kkcflmx > 0) then
       numlimdt(kkcflmx) = numlimdt(kkcflmx) + 1
    endif

    if (jatimestepanalysis == 1) then
       if (mout == 0) then
          call newfil(mout, trim(md_ident)//'.steps')
          write(mout, '(A)')  'time0/60, dts, dtsc, kkcflmx, kcflmx-kbot(kkcflmx)+1, vol1(kcflmx), squ2D(kcflmx), squ(kcflmx), sqi(kcflmx) '
       endif
       if (kkcflmx > 0) then
          if (kcflmx == 0) kcflmx = kkcflmx
          if (ja_timestep_auto == 3 .or. ja_timestep_auto == 4 ) then
             write(mout, '(3F14.4,2I8,4F14.4)')  time0/60d0, dts, dtsc, kkcflmx, kcflmx-kbot(kkcflmx)+1, vol1(kcflmx), squ2D(kkcflmx), squ(kcflmx), sqi(kcflmx)
          else
             write(mout, '(3F14.4,2I8,4F14.4)')  time0/60d0, dts, dtsc, kkcflmx, kcflmx-kbot(kkcflmx)+1, vol1(kcflmx), squ  (kcflmx), squ(kcflmx), sqi(kcflmx)
          endif
       else
          write(mout, '(3F14.4, I8)')         time0/60d0, dts, dtsc, kkcflmx
       endif
    endif

    return
 end subroutine timestepanalysis
