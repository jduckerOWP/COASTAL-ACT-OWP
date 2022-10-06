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

! $Id: setcentertolinkorientations.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/setcentertolinkorientations.f90 $

 subroutine setcentertolinkorientations()
    use m_flowgeom
    use network_data, only: xk, yk
    use m_sferic
    use m_alloc
    use unstruc_messages
    use geometry_module, only :half, spher2locvec
    use m_missing, only : dmiss

    implicit none

    double precision               :: xL, yL

    integer                        :: i, k, k3, k4
    integer                        :: L

    integer                        :: ierr

    double precision, parameter    :: dtol = 1d-8

    if ( allocated (csb) ) deallocate(csb)
    if ( allocated (snb) ) deallocate(snb)

    if ( jsferic.eq.0 .or. jasfer3D.eq.0 ) return

    allocate ( csb(2,Lnx) , stat  = ierr) ; csb = 1d0
    call aerr('csb(2,Lnx)', ierr, 2*Lnx)
    allocate ( snb(2,Lnx) , stat  = ierr) ; snb = 0d0
    call aerr('snb(2,Lnx)', ierr, 2*Lnx)

    do L=1,Lnx
       if ( L.eq.12 ) then
          continue
       end if
       k3    = lncn(1,L)
       k4    = lncn(2,L)

!      compute flowlink midpoint coordinates (xL,yL)
       call half(xk(k3),yk(k3),xk(k4),yk(k4),xL,yL, jsferic, jasfer3D)

       do i=1,2
          k = ln(i,L)

!         compute orientation w.r.t. link mid point
          call spher2locvec(xz(k),yz(k),1,(/xL/),(/yL/),(/1d0/),(/0d0/),csb(i,L),snb(i,L),jsferic, jasfer3D, dmiss)
       end do
    end do

    return
 end subroutine setcentertolinkorientations
