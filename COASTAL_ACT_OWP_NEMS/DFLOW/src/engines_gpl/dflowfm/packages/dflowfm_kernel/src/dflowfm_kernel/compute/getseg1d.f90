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

! $Id: getseg1d.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/getseg1d.f90 $

subroutine getseg1D(hpr,wu2,dz,ai,frcn,ifrctyp, wid,ar,conv,perim,jaconv)  ! copy of above routine dressed out for 1D
 implicit none
 double precision, intent (in)  ::     hpr,wu2,dz,ai,frcn
 double precision, intent (out) ::                                 wid,ar,conv,perim        !
 integer,          intent (in)  ::                         ifrctyp                   ,jaconv
 double precision  :: d83 = 2.666666d0, d16 = 0.166666d0 , d23 = 0.666666d0, d43= 1.333333d0
 double precision  :: tt, hp2, hrad, Cz, cman, per, hav
 double precision  :: d38 = 0.375d0 , d113 = 3.666666d0 , d311 = 0.27272727d0, hpr83, hp283, d14 = 0.25d0
 integer           :: jac, L

 ! for jaconv >= 1, this routine gets 1D conveyance
 ! this constant value, (1+(dz/dy)**2)**0.25 is computed once and is volume cell based instead of link based

 if (ai < 1d-3) then
    wid = wu2
    ar  = wid * hpr
 else if (hpr < dz) then
    wid = wu2 * hpr / dz
    ar  = 0.5d0*wid*hpr
 else
    wid = wu2
    hp2 = hpr - dz
    ar  = wid*0.5d0*(hpr + hp2)
 endif

 if (jaconv == 0) then
    return
 else if (frcn == 0d0) then
    conv = 0d0 ;  return
 else if (jaconv == 1) then                       ! hydraulic radius type

    if (ai < 1d-3) then
       perim = wid
    else if (hpr < dz) then
       perim = sqrt(wid*wid + hpr*hpr)
    else
       perim = sqrt(wid*wid + (hpr-hp2)*(hpr-hp2) )
    endif

 else if (jaconv >= 2) then                       ! 1D analytic conveyance type
    if (ifrctyp == 1) then
       cman = frcn
    else
       if (ai < 1d-3) then
          hav = hpr
       else if (hpr < dz) then
          hav = 0.5d0*hpr
       else
          hav = hpr -0.5d0*dz
       endif
       call getcz(hav, frcn, ifrctyp, Cz, L)
       cman = hav**d16/Cz
    endif

    if (ai < 1d-3 ) then                       ! see sysdoc 5 1D conveyance
       conv = ( ar *hpr**d23             ) / ( cman                     )
    else if (hpr < dz) then
       conv = ( d38*hpr**d83             ) / ( cman*ai*(1d0+ai*ai)**d14 )
    else
       conv = ( d38*(hpr**d83-hp2**d83)  ) / ( cman*ai*(1d0+ai*ai)**d14 )
    endif

 endif
 end subroutine getseg1D
