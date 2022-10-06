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

! $Id: flow_settidepotential.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/timespace/flow_settidepotential.f90 $

 subroutine flow_settidepotential(timmin)
 use m_flow
 use m_flowgeom
 use m_flowtimes
 use timespace_data
 use m_sferic
 use unstruc_model
 use m_equatorial

 implicit none

 double precision :: timmin

 integer, save    :: ini = 0
 integer          :: ierr, kk

 double precision :: Omeg, tt

 call meteo_tidepotential( julrefdat, TIMmin , xz , yz , size(tidep,1), tidep, ndx, doodsonstart, doodsonstop , doodsoneps)

 if (md_ident == 'equator1d' ) then
    tt   = 60d0*(timmin-tstart_user)
    do kk = 1,ndx
       tidep(1,kk) = ZP*sin(om*tt - nmode*dg2rd*xz(kk) )
    enddo
 endif

 end subroutine flow_settidepotential
