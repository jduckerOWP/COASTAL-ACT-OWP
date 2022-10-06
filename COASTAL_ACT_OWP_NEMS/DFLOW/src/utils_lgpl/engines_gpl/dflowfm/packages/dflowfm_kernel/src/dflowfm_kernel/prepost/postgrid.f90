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

! $Id: postgrid.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/postgrid.f90 $

!> remove skewed cells and cells whose aspect ratio exceeds a prescibed value
!> note: latter not implemented yet
subroutine postgrid()

   use m_grid
   use m_spline2curvi, only: maxaspect
   use m_missing
   use geometry_module, only: dbdistance, dcosphi
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   integer,          dimension(mc)                   :: ifront

   double precision                                  :: dh, daspect, dcos, dcosR, xn, yn

   integer                                           :: i, iL, iR, iRR, idum, iL0, iR0, i1, j, ja, iter, numchanged

   integer                                           :: istriangle   ! 0: no, -1: left, 1: right, 2: two

!   double precision, parameter                       :: dcosmax =  0.86603
   double precision, parameter                       :: dcosmax =   0.93969
   double precision, parameter                       :: dtol    = 1d-2
   double precision, parameter                       :: dtolcos = 1d-2

   call tekgrid(i)

   ja = 1
   call confrm('Remove skinny triangles?', ja)

   if ( ja.eq.1 ) then
!     remove skewed cells
      do j=nc-1,2,-1
         ifront = 1
         do iter=1,10
            write(6,"('iter = ', i0, ': ', $)") iter
            numchanged = 0
   !        loop over the edges
   !         do i=1,mc-1
            iR = 1
            i  = iR
            do while (iR.ne.mc .or. i.ne.mc )
               if ( iR.gt.i ) then
                  i = iR
               else
                  i = i+1
                  if ( i.ge.mc ) exit
               end if

               if ( xc(i,j).eq.DMISS ) cycle

               call get_LR(mc, xc(:,j), yc(:,j), i, iL, iR)

               if ( dbdistance(xc(i,j),yc(i,j),xc(iR,j),yc(iR,j),jsferic, jasfer3D, dmiss).lt.dtol ) cycle

         !        detect triangular cell
               if ( xc(i,j+1).eq.DMISS ) cycle

               call get_LR(mc, xc(:,j+1), yc(:,j+1), i, iL0, iR0)

               if ( dbdistance(xc(iL,j),yc(iL,j),xc(i,j),yc(i,j),jsferic, jasfer3D, dmiss).lt.dtol ) iL=i

               if ( xc(iR,j+1).ne.DMISS ) then
                  if ( dbdistance(xc(i,j+1),yc(i,j+1),xc(iR,j+1),yc(iR,j+1),jsferic, jasfer3D, dmiss).lt.dtol .and.   &
                       dcosphi(xc(i,j+1),yc(i,j+1),xc(i,j),yc(i,j),xc(i,j+1),yc(i,j+1),xc(iR,j),yc(iR,j),jsferic, jasfer3D, dxymis).gt.dcosmax ) then
         !              determine persistent node
                     dcos  = dcosphi(xc(i,j-1),yc(i,j-1),xc(i,j),yc(i,j),xc(i,j),yc(i,j),xc(i,j+1),yc(i,j+1), jsferic, jasfer3D, dxymis)
                     dcosR = dcosphi(xc(iR,j-1),yc(iR,j-1),xc(iR,j),yc(iR,j),xc(iR,j),yc(iR,j),xc(iR,j+1),yc(iR,j+1),  jsferic, jasfer3D, dxymis)

                     call get_LR(mc, xc(:,j), yc(:,j), iR, idum, iRR)
                     if ( (iRR.eq.iR .or. dcos-dcosR.lt.-dtolcos) .and. iL.ne.i ) then ! move left node
                        call cirr(xc(i,j),yc(i,j),211)
                        call cirr(xc(iR,j),yc(iR,j),31)
                        xc(i:iR-1,j) = xc(iR,j)
                        yc(i:iR-1,j) = yc(iR,j)
                        numchanged = numchanged+1
                        write(6,"(I0, '-', I0, 'L ', $)") i, iR-1
                     else if ( ( iL.eq.i .or. dcosR-dcos.lt.-dtolcos) .and. iRR.ne.iR ) then  ! move right node
                        call cirr(xc(iR,j),yc(iR,j),211)
                        call cirr(xc(i,j),yc(i,j),204)
                        xc(iR:iRR-1,j) = xc(i,j)
                        yc(iR:iRR-1,j) = yc(i,j)
                        numchanged = numchanged+1
                        write(6,"(I0, '-', I0, 'R ', $)") iR, iRR-1
                     else  ! move both nodes
                        xn = 0.5d0*(xc(i,j)+xc(iR,j))
                        yn = 0.5d0*(yc(i,j)+yc(iR,j))
                        call cirr(xn,yn,211)
                        xc(i:iR-1,j) = xn
                        yc(i:iR-1,j) = yn
                        xc(iR:iRR-1,j) = xn
                        yc(iR:iRR-1,j) = yn
                        numchanged = numchanged+1
                        write(6,"(I0, '-', I0, 'C ', $)") i, iRR-1
                     end if
                  end if
               end if
            end do
            write(6,*)
            if ( numchanged.eq.0 ) exit
         end do
         write(6,*) iter, numchanged
      end do
   end if

   return
end subroutine postgrid
