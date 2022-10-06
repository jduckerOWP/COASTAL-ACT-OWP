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

! $Id: connecthangingnodes.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_rgf/connecthangingnodes.f90 $

subroutine connecthangingnodes()
use m_netw
use m_flowgeom
use m_missing
use gridoperations
implicit none

integer :: mout, np, ih, kk, k, kk3, kkx, k1,k2,lnu, km, kp, i

call findcells(0)
call newfil(mout, 'hang.xyz')

lnu =numL
do np  = 1,nump
   kk3 = 0
   kkx = netcell(np)%n
   if (kkx <= 4) then
      cycle
   endif
   do kk = 1,netcell(np)%n
      k  =netcell(np)%nod(kk)
      if (nmk(k) == 3) then
         km = kk - 1; if (km < 1  ) km = km + kkx
         kp = kk + 1; if (kp > kkx) kp = kp - kkx
         km = netcell(np)%nod(km)
         kp = netcell(np)%nod(kp)
         if (abs(yk(km) - yk(k)) < 1d-10 .and. abs (yk(kp) - yk(k)) < 1d-10  .or. & 
             abs(xk(km) - xk(k)) < 1d-10 .and. abs (xk(kp) - xk(k)) < 1d-10) then
            km  = kk - 2; if (km < 1)   km = km + kkx
            kp  = kk + 2; if (kp > kkx) kp = kp - kkx
            km  = netcell(np)%nod(km)
            kp  = netcell(np)%nod(kp)
            lnu = lnu + 1
            kn(1,lnu) = k ; kn(2,lnu) = km; kn(3,lnu) = 2
            lnu = lnu + 1
            kn(1,lnu) = k ; kn(2,lnu) = kp; kn(3,lnu) = 2
            !call connectdbn(k,km,lnu)
            !call connectdbn(k,kp,lnu)
         endif
      endif
   enddo
enddo
numL = Lnu
call doclose(mout)
call findcells(0)

end subroutine connecthangingnodes

subroutine removelinksofhangingnodes()
use m_netw
use m_flowgeom

implicit none

integer :: L, k1, k2 

do L = 1,numL
   k1 = kn(1,L) ; k2 = kn(2,L)
   if (abs(xk(k1)-xk(k2)) > 1d-10 .and. abs(yk(k1)-yk(k2)) > 1d-10) then  
      kn(1,L) = 0 ; kn(2,L) = 0 ; kn(3,L) = 0
   endif
enddo

call setnodadm(0)
end subroutine removelinksofhangingnodes

