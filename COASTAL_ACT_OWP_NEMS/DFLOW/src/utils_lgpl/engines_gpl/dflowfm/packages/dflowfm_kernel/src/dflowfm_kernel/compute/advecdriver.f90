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

! $Id: advecdriver.f90 140626 2022-01-13 12:22:15Z nabi $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/advecdriver.f90 $

 subroutine advecdriver()
 use m_flowtimes
 use m_flow
 use m_flowgeom
 use m_setucxcuy_leastsquare, only: reconst2nd
 implicit none

 double precision :: dta, das, ds
 integer          :: L, k1, k2, k

 if (itstep == 3) then

   if (.not. allocated(adve0) ) then
      allocate(adve0(lnkx))
   endif

   dta = 0.7D0*dts/cflmx
   das = dta/dts
   do k = 1,2

      adve = 0d0

      call advec()
      if (k == 1) then
         adve0 = adve
      endif
      do L = 1,lnx
         k1 = ln(1,L) ; k2 = ln(2,L)
         ds = ag*dxi(L)*(s0(k2) - s0(k1))
         u1(L) = ( u1(L)*(1d0 - das) + u0(L)*das - dta*(adve(L) + ds) ) / (1d0 + dta*advi(L))
      enddo
      if (iperot == -1) then
         call reconst2nd()
      endif
      call setucxucyucxuucyunew()

   enddo
   ! adve = teta0*adve + (1d0-teta0)*adve0
   ! u1 = u0

 else

   call advec()                                       ! advection term, must be called after set-umod and cell velocity updates

 endif

 call setextforcechkadvec()                           ! set external forcings and check explicit part adve
 end subroutine advecdriver
