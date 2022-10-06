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

! $Id: tekpolygon.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/tekpolygon.f90 $

   subroutine tekpolygon()
   use m_polygon
   use unstruc_display
   use m_missing

   implicit none

   integer           :: k,ncol,kk, key,k2
   double precision  :: a,b,f,x,y,z,s,c,d,dx,dy,dz,dc,dl,dr,ds,dxL,dyL,dxR,dyR,sL,sR, dcxR, dcyR, dcxL, dcyL
   logical inview

   if (ndrawpol == 2) then

      CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLPL)

   else if (ndrawpol == 3) then

      CALL DISP2C(XPL, YPL, NPL, 0d0, NCOLPL)

   else if (ndrawpol == 4) then

      do k = 1,npl-1
         if (zpl(k) .ne. dmiss .and. zpl(k+1) .ne. dmiss) then
            call isoline( xpl(k), ypl(k), zpl(k), xpl(k+1), ypl(k+1), zpl(k+1) )
         endif
      enddo

   else if (ndrawpol >= 5 .and. ndrawpol <= 10) then

      CALL DISP2C(XPL, YPL, NPL, 0d0, NCOLPL)
      CALL SETCOL(NCOLBLACK)
      do k = 1,npl
         if ( inview(xpl(k), ypl(k) ) ) then
            if ( ndrawpol == 5) then
               call HTEXT(Zpl(k),Xpl(k),Ypl(k))
            else if ( ndrawpol == 6 .and. jakol45 > 0) then
               call HTEXT(dcrest(k),Xpl(k),Ypl(k))
            else if ( ndrawpol == 7 .and. jakol45 > 0) then
               call HTEXT(dzL(k),Xpl(k),Ypl(k))
            else if ( ndrawpol == 8 .and. jakol45 > 0) then
               call HTEXT(dzr(k),Xpl(k),Ypl(k))
            else if ( ndrawpol == 9 .and. jakol45 > 0) then
               call HTEXT(dtL(k),Xpl(k),Ypl(k))
            else if ( ndrawpol == 10 .and. jakol45 > 0) then
               call HTEXT(dtR(k),Xpl(k),Ypl(k))

            endif
         endif
      enddo

     else if (ndrawpol == 11 .and. jakol45 > 0) then

      do k = 1,npl-1

         IF (MOD(k,100) ==  0) THEN
             CALL HALT2(KEY)
             IF (KEY .EQ. 1) RETURN
         ENDIF

         if (xpl(k) .ne. dmiss .and. xpl(k+1) .ne. dmiss ) then
            if ( inview( xpl(k), ypl(k) ) .or. inview ( xpl(k+1), ypl(k+1) )  ) then
               call isoline( xpl(k), ypl(k), zpl(k), xpl(k+1), ypl(k+1), zpl(k+1) )

               call sincosdis (xpl(k), ypl(k), xpl(k+1), ypl(k+1), s, c, d)

               dy =  rcir*c
               dx = -rcir*s

               k2 = max(2, int (d /(3d0*rcir)) )
               do kk = 1, k2
                  a  = 1d0 - dble(kk)/dble(k2)
                  b  = 1d0-a
                  x  = a*xpl(k) + b*xpl(k+1)
                  y  = a*ypl(k) + b*ypl(k+1)
                  z  = a*zpl(k) + b*zpl(k+1)

                  dc = a*dcrest(k) + b*dcrest(k+1) ; dc = 0.5d0*dc ! crest width
                  dy =  dc*c ; dcyR = dy ; dcyL = dy
                  dx = -dc*s ; dcxR = dx ; dcxL = dx

                  dL = a*dzL(k) + b*dzL(k+1)  ! step left
                  dR = a*dzR(k) + b*dzR(k+1)  ! step right

                  sL = a*dtL(k) + b*dtL(k+1)  ! slope left
                  sR = a*dtR(k) + b*dtR(k+1)  ! slope right

                  if (dL > 0d0 .and. dR == 0d0) then ! baseline is probably wrong with slope, set 1 instead of 10
                     sL = 1d0 ; dcxL = 0 ; dcyL = 0d0
                  endif

                 if (dR > 0d0 .and. dL == 0d0) then ! baseline is probably wrong with slope, set 1 instead of 10
                     sR = 1d0 ; dcxR = 0 ; dcyR = 0d0
                  endif


                  dc  = dR*sR
                  dyR =  dc*c
                  dxR = -dc*s
                  call isoline( x-dcxR, y-dcyR, z, x         , y         , z   ) ! half crest width to right
                  if (dR == 0d0) then
                      call rcirc(x-dcxR, y-dcyR)
                  else
                      call isoline( x-dcxR, y-dcyR, z, x-dcxR-dxR, y-dcyR-dyR, z-dR) ! slope to right
                  endif

                  dc  = dL*sL
                  dyL =  dc*c
                  dxL = -dc*s
                  call isoline( x+dcxL, y+dcyL, z, x         , y         , z   ) ! half crest width to left
                  if (dL == 0d0) then
                      call rcirc(x+dcxL, y+dcyL)
                  else
                      call isoline( x+dcxL, y+dcyL, z, x+dcxL+dxL, y+dcyL+dyL, z-dL)
                  endif

               enddo
            endif
         endif
      enddo


    else if ( ndrawpol == 12 ) then

         CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLPL)
         do k = 1,npl
            if ( inview(xpl(k), ypl(k) ) ) then
               call HTEXT(dble(k),Xpl(k),Ypl(k))
            endif
         enddo
         call hTEXT(dble(k),Xpl(k),Ypl(k))

   endif
   end subroutine tekpolygon
