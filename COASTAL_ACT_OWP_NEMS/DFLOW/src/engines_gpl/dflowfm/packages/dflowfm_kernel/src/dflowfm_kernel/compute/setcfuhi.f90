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

! $Id: setcfuhi.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/setcfuhi.f90 $

 subroutine setcfuhi()                                 ! set friction coefficients g/C2 etc
 use m_flowtimes                                       ! sqrt(g/C2) in both in 2D and in 3D
 use m_flow
 use m_flowgeom
 use m_missing

 implicit none

 ! locals
 double precision ::  h0, dzb, cz, sixth = 1d0/6d0, frcn, z00, sqcf
 integer :: l, ll, n, kb, Lb, ifrctyp

 ! NOTE: When frcuni==0, the initial friction fields in frcu also become noneffective:
 if ( jatrt.eq.0 .and. (frcmax == 0d0 .or. ifrctypuni == -999) ) then
    cfuhi = 0 ; return
 endif
 if (jaconveyance2D >= 1 ) then ! .and. kmx <=1 ) then
    return
 endif

 if (kmx <= 1) then                                  ! 2D
    if (ifrctypuni == 4) then
       cfuhi = 0
    else

       !$OMP PARALLEL DO                             &
       !$OMP PRIVATE(L,h0,frcn,cz)
       do L = lnx1D+1,lnx
          if (hu(L) > 0) then
             if (jaconveyance2D == 0) then           ! original default
                h0 = max(epshs, 1d0 / huvli(L))
             else if (jaconveyance2D == -1) then     ! better for straight test
                h0 = max(epshs, hu(L)) ! does it whole not
             endif
             frcn = frcu(L)
             if ( frcn.gt.0d0 ) then
                call getcz(h0, frcn, ifrcutp(L), cz, L)
                cfuhi(L) = ag/(h0*cz*cz)
             else
                cfuhi(L) = 0d0
             end if
          endif
       enddo
       !$OMP END PARALLEL DO

    endif

 endif

 end subroutine setcfuhi
