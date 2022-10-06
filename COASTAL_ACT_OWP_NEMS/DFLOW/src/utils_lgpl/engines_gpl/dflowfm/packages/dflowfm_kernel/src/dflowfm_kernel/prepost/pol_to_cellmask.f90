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

! $Id: pol_to_cellmask.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/pol_to_cellmask.f90 $

!> update cellmask from samples
!> a cell is dry when it is:
!>   1) inside ANY "1"-polygon (drypnts), OR
!>   2) outside ALL "-1"-polygons (enclosures)
subroutine pol_to_cellmask()
   use network_data
   use m_polygon
   use m_missing, only: dmiss, JINS
   use geometry_module, only: dbpinpol_optinside_perpol

   implicit none

   integer                                     :: i, ic, in, k, KMOD
   integer                                     :: num

   if ( allocated(cellmask) ) deallocate(cellmask)
   allocate(cellmask(nump1d2d))
   cellmask = 0

   if ( NPL.eq.0 ) return  ! no polygon

   CALL READYY('Applying polygon cellmask',0d0)
   KMOD = MAX(1,NUMP/100)

   !  generate cell mask
   in = -1
   do k = 1,nump
!     check if cell is in any "zpl>0" polygons
      call dbpinpol_optinside_perpol(xzw(k), yzw(k), 0, 1, in, num, dmiss, JINS, NPL, xpl, ypl, zpl)

      if ( in.eq.0 ) then
!        check if cell is outside all "zpl<0" polygons (enclosure)
         call dbpinpol_optinside_perpol(xzw(k), yzw(k), 0, -1, in, num, dmiss, JINS, NPL, xpl, ypl, zpl)   ! in=0: outside all "-1"-pol
         if ( num.gt.0) in = 1-in   ! only if at least one "-1"-type polygon was encountered
      end if

      if ( in.gt.0 ) then
!        mask cell
         cellmask(k) = 1
      end if
      IF (MOD(k,KMOD) .EQ. 0) THEN
         CALL READYY(' ',MIN( 1d0,dble(k)/nump ) )
      ENDIF
   end do
   CALL READYY(' ',-1d0)

   return
end subroutine pol_to_cellmask
