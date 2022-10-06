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

! $Id: admin_landboundary_segments.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/admin_landboundary_segments.f90 $

!> administer the land boundary segments
!>   the land boundary will be split into segments that are
!>      within the selecting polygon, and
!>      either close to the net boundary, or
!>      not close to the net boundary
subroutine admin_landboundary_segments()
   use m_landboundary
   use m_polygon
   use m_alloc
   use m_netw
   use m_missing, only: dmiss, JINS
   use geometry_module, only: dbpinpol, dbdistance
   use m_sferic, only: jsferic, jasfer3D

   implicit none
   integer, allocatable, dimension(:) :: lanmask ! mask the parts of the landboundary that are within the polygon
                                                 !   0: inactive
                                                 !  -1: active, not member of an edge close to land boundary
                                                 !   1: active, member of an edge close to the net boundary

   integer                            :: jstart, jend
   integer                            :: jbreak       ! used to break segment in two
   integer                            :: i, j, jdum, ja, ja1, ja2, k, N, Nnew

   double precision                   :: x1,y1,x2,y2,x3,y3,x4,y4,xn,yn,rl3,rl4
   double precision                   :: dlanlength,dlinklength,dismin,dis3,dis4
   double precision                   :: darea, dlength, dlenmx

   logical                            :: Lisclose

!  allocate
   if ( allocated(lanseg_startend) ) deallocate(lanseg_startend)

   allocate(lanseg_startend(2,1) )

   allocate(lanmask(MXLAN-1))

!  mask the landboundary that is inside the selecting polygon
!     lanmask masks the landboundary edges
!     mask set to -1
   ja1 = -1
   ja2 = -1
   lanmask = 0
   do i=1,MXLAN-1
      if ( xlan(i).ne.DMISS .and. xlan(i+1).ne.DMISS ) then
         call dbpinpol(xlan(i),ylan(i),ja1,dmiss, JINS, NPL, xpl, ypl, zpl)
         call dbpinpol(xlan(i+1),ylan(i+1),ja2,dmiss, JINS, NPL, xpl, ypl, zpl)
!         if ( ja1.eq.1.or.ja2.eq.1 ) then
!            lanmask(i)   = -1
!            lanmask(i+1) = -1
         if ( ja1.eq.1.or.ja2.eq.1 ) then
            lanmask(i)   = -1
         end if
      end if
   end do

!  mask the landboundary that is sufficiently close to the net
!     mask set to 1

!  save the selecting polygon
   call savepol()
!  copy network boundary to polygon
   call copynetboundstopol(1, 0, 0, 1)

   do i=1,MXLAN-1
      if ( lanmask(i).ne.0 ) then   ! segments in polygon only
!        land boundary points
         x1 = xlan(i)
         y1 = ylan(i)
         x2 = xlan(i+1)
         y2 = ylan(i+1)

         dlanlength = dbdistance(x1,y1,x2,y2, jsferic, jasfer3D, dmiss)

         Lisclose = .false.
         do j=1,NPL-1   ! loop over the network boundary
            x3 = xpl(j)
            y3 = ypl(j)
            x4 = xpl(j+1)
            y4 = ypl(j+1)

            if ( x3.eq.DMISS .or. x4.eq. DMISS ) cycle

            dlinklength = dbdistance(x3,y3,x4,y4, jsferic, jasfer3D, dmiss)

!            dismin = DCLOSE*min(dlanlength,dlinklength)
            dismin = DCLOSE*dlinklength

            call dlinedis3(x3,y3,x1,y1,x2,y2,ja,dis3,xn,yn,rl3)
            call dlinedis3(x4,y4,x1,y1,x2,y2,ja,dis4,xn,yn,rl4)
            if ( dis3.le.dismin .or. dis4.le.dismin ) then
               Lisclose = .true.
               exit
            end if
         end do

         if ( Lisclose ) then
            lanmask(i)   = 1
!            lanmask(i+1) = 1
         end if
      end if
   end do

!  restore the selecting polygon
   call restorepol()

!  compose the boundary segments that have same lanmask
   Nlanseg    = 0
   jend       = 1
   do while( jend.lt.MXLAN )
      Nlanseg = Nlanseg+1

!     find jstart and jend
      jstart = jend
      if ( xlan(jstart+1).eq.DMISS ) jstart = jstart+1
      if ( jstart.ge.MXLAN ) exit
      do while( xlan(jstart).eq.DMISS )
         jstart = jstart+1
         if ( jstart.eq.MXLAN ) exit
      end do
      if ( xlan(jstart).eq.DMISS ) exit

      i = lanmask(jstart)
      jend   = jstart+1
      if ( jend.lt.MXLAN ) then
         do while( (xlan(jend+1).ne.DMISS .and. lanmask(jend).eq.i) )
            jend = jend+1
            if ( jend.eq.MXLAN ) exit
         end do
      end if

!     only store landboundary segments that are inside the selecting polygon
      if ( lanmask(jstart).ne.0 ) then
   !     allocate and administer
         call realloc(lanseg_startend, (/ 2, Nlanseg /))
         lanseg_startend(:,Nlanseg) = (/ jstart, jend /)
      else
         Nlanseg = Nlanseg-1
      end if
   end do

!  count number of segments
   if ( jend.gt.0 ) then
      Nlanseg = ubound(lanseg_startend,2)
   else
      Nlanseg = 0
   end if

!  split the line segments into two to accommodate closed segments
!     28-10-11: deactivated, since the link distance (weight) embodies the maximum distance to the landboudary path between the projected begin and end node of the link
!               unwanted paths could possibly be found under certain conditions
!     21-11-11: activated again
   if ( Nlanseg.gt. 0 ) then
      Nnew = Nlanseg
      do i=1,Nlanseg
         jstart = lanseg_startend(1, i)
         jend   = lanseg_startend(2, i)
         if ( jend-jstart.gt.2 ) then
!           only if the distance from begin to end of the landboundary is a fraction (one tenth) of the segment length
!            call darean(xlan(jstart:jend), ylan(jstart:jend), jend-jstart+1, darea, dlength, dlenmx)
!            if ( dbdistance(xlan(jstart),ylan(jstart),xlan(jend),ylan(jend)).lt.0.1d0*dlength ) then
               Nnew = Nnew+1
               call realloc(lanseg_startend, (/ 2, Nnew /) )
               jbreak = jstart + ( jend - jstart ) / 2
               lanseg_startend(2,i)    = jbreak
               lanseg_startend(1,Nnew) = jbreak
               lanseg_startend(2,Nnew) = jend
!            end if
         end if
      end do
      Nlanseg = Nnew
   end if

!  deallocate
   deallocate(lanmask)

   return
end subroutine admin_landboundary_segments
