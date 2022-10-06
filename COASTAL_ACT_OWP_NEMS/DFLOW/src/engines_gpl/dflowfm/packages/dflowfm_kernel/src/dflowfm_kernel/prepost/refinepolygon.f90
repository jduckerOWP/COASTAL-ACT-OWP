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

! $Id: refinepolygon.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/refinepolygon.f90 $

!> Refine entire current polyline from start to end.
      subroutine refinepolygon()
      use m_polygon  !, only: npl, dxuni
      use m_tpoly
      use m_sferic
      use m_missing
      use geometry_module, only: dbdistance, half
      implicit none
      integer :: i1, i2
      integer :: key

      type(tpoly), dimension(:), allocatable :: pli, pliout              ! tpoly-type polygons

      double precision                       :: dl, xnew, ynew, znew

      integer                                :: numpols, numpolsout      ! number of tpoly-type polygons
      integer                                :: i
      integer                                :: iter, j
      integer                                :: M, NPUT

      i1 = 1
      i2 = npl
      call refinepolygonpart(i1,i2,0)

      call TYPEVALUE(dxuni,key)

      call pol_to_tpoly(numpols, pli, keepExisting=.false.)
      call delpol()

      write(6,*) numpols
      do i=1,numpols
         write(6,*) i
         call tpoly_to_pol(pli,iselect=i)
!         i1 = 1
!         i2 = NPL
!         call refinepolygonpart(i1,i2,1)

!        loop over polygon points
         j = 1
         do while ( j.lt.NPL )
!           get length
            dl = dbdistance(xpl(j), ypl(j), xpl(j+1), ypl(j+1), jsferic, jasfer3D, dmiss)

!           check length
            if ( dl.gt.dxuni ) then
!              compute new point coordinates
               call half(xpl(j), ypl(j), xpl(j+1), ypl(j+1),xnew,ynew,jsferic,jasfer3D)
               znew = DMISS
               if ( zpl(j).ne.DMISS .and. zpl(j+1).ne.DMISS ) then
                  znew = 0.5*(zpl(j)+zpl(j+1))
               end if
!              add point
               call increasepol(NPL+1, 1)
               NPUT = -1
               M = j
               CALL MODLN2(XPL, YPL, ZPL, MAXPOL, NPL, M, xnew, ynew, NPUT)
               ZPL(M) = znew
            else
               j = j+1
            end if
         end do

         call pol_to_tpoly(numpolsout, pliout, keepExisting=.true.)
         call delpol()
      end do

      call tpoly_to_pol(pliout)
      call dealloc_tpoly(pli)
      call dealloc_tpoly(pliout)

      end subroutine refinepolygon
