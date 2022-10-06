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

! $Id: caldat.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/to_deltares_common/caldat.f90 $

 subroutine caldat(julian,mm,id,iyyy)
 implicit none
 integer :: julian,mm,id,iyyy
 integer :: igreg
 parameter (igreg=2299161)
 integer :: jalpha, ja, jb, jc, jd, je
 if(julian.ge.igreg)then
   jalpha=int(((julian-1867216)-0.25)/36524.25)
   ja=julian+1+jalpha-int(0.25*jalpha)
 else
   ja=julian
 endif
 jb=ja+1524
 jc=int(6680.+((jb-2439870)-122.1)/365.25)
 jd=365*jc+int(0.25*jc)
 je=int((jb-jd)/30.6001)
 id=jb-jd-int(30.6001*je)
 mm=je-1
 if(mm.gt.12)mm=mm-12
 iyyy=jc-4715
 if(mm.gt.2)iyyy=iyyy-1
 if(iyyy.le.0)iyyy=iyyy-1
 return
 end subroutine caldat
