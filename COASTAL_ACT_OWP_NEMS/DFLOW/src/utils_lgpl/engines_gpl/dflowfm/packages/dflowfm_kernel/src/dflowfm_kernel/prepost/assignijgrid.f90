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

! $Id: assignijgrid.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/assignijgrid.f90 $

!>  assign indices (i,j) to the curvi-linear grid
subroutine assignijgrid(k, ic, jc)

 use m_netw
 use m_grid
 use m_missing

 implicit none

 integer                  :: k                           !< current cell

 integer, dimension(numk) :: ic, jc                      !< indices (i,j) of the nodes

 integer                  :: kcell, kneighbor, kdir, kdirdum
 integer                  :: icount, iter, lowold(2), uppold(2)
 integer, parameter       :: MAXITER = 1000000
! integer, parameter       :: IMISS   = -999999

 integer                  :: i, numiter_guess
!---------------------------------------------------------
! cellmask
!   -1 : inactive,     in curvi-grid
!    0 : inactive, not in curvi-grid
!    1 :   active, not in front
!   >1 :   active,     in front
!---------------------------------------------------------

! mask current cell as frontcell
 cellmask(k) = 10

 lowold = lbound(ijc)
 uppold = ubound(ijc)

 numiter_guess = sqrt(dble(nump)) * 10

 call readyy('creating curvilinear grid', 0d0 )

 do iter = 1,MAXITER
    call readyy('creating curvilinear grid', min(dble(iter-1)/dble(numiter_guess-1), 1d0) )

    icount = 0

    do kcell = 1,nump
       if ( cellmask(kcell) .eq. iter+9 ) then

! done with cell kcell - unmask cell
          cellmask(kcell) = -1

          do kdir = 1,4

             call assignij(kcell, kdir, kneighbor, ic, jc)

             if ( kneighbor.ne.0 ) then
                if ( cellmask(kneighbor).eq.1 ) then
                   cellmask(kneighbor)    = iter + 10
                   icount                 = icount + 1
                end if
             end if
          end do
       end if
    end do


! only one layer of cells will be added during the next iteration at maximum
    call grow_ijc( lowold, uppold,                                                     &
                         (/ minval(ic, ic.ne.IMISS)-1, minval(jc, jc.ne.IMISS)-1 /),   &
                         (/ maxval(ic, ic.ne.IMISS)+1, maxval(jc, jc.ne.IMISS)+1 /), 0)

    if ( icount .eq.0 ) exit
 end do

 if ( iter.eq.MAXITER ) write(6,*) 'assignijgrid: iter=MAXITER'

 call readyy('creating curvilinear grid', -1d0 )

end subroutine assignijgrid
