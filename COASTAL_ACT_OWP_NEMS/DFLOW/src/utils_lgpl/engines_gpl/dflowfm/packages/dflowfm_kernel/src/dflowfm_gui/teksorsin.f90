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

! $Id: teksorsin.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/teksorsin.f90 $

subroutine teksorsin()      ! teksrc
use m_flowexternalforcings
use unstruc_display
use m_transport, only: isalt, itemp

implicit none
COMMON /DRAWTHIS/  ndraw(50)
integer           :: ndraw
integer           :: n, k, kb, kt, n2, ncol
character*40      :: tex
double precision  :: znod, temb, temt, xp, yp
logical inview


if (ndraw(41) <= 1 .or. numsrc == 0) return

call IGrCharJustify('L')
call settextsizefac(1.0d0)

do n = 1,numsrc ! teksorsin
   k = ksrc(1,n)
   if (k .ne. 0) then
      n2 = 1 ; xp = xsrc(n,n2) ; yp = ysrc(n,n2)
      if ( inview(xp,yp) ) then
          if (qsrc(n) > 0) then
             ncol = 3
          else
             ncol = 221
          endif
          call cirr(xp, yp, ncol)
          if (ndraw(41) == 3) then
             call gtext(' '//trim(srcname(n)), xp, yp , klsrc)
          else if (ndraw(41) == 4) then
             write(tex,'(f10.3)') -qsrc(n)
             call gtext(trim(tex)//' (m3/s)', xp, yp , klsrc)
          else if (ndraw(41) == 5.and. isalt > 0) then
             if (qsrc(n) < 0d0) then
                write(tex,'(f10.3)') ccsrc(isalt,n)
                call gtext(trim(tex)//' (ppt)', xp, yp , klsrc)
             endif
          else if (ndraw(41) == 6 .and. itemp > 0) then
             if (qsrc(n) < 0d0) then
                write(tex,'(f10.3)') ccsrc(itemp,n)
                call gtext(trim(tex)//' (degC)', xp, yp , klsrc)
             endif
          endif
      endif
   endif
   k = ksrc(4,n)
   if (k .ne. 0) then
      n2 = nxsrc(n) ; xp = xsrc(n,n2) ; yp = ysrc(n,n2)
      if ( inview(xp,yp) ) then
          if (qsrc(n) > 0) then
             ncol = 221
          else
             ncol = 3
          endif
          call cirr(xp, yp, ncol)
          if (ndraw(41) == 3) then
             call gtext(' '//trim(srcname(n)), xp, yp , klsrc)
          else if (ndraw(41) == 4) then
             write(tex,'(f10.3)') qsrc(n)
             call gtext(trim(tex)//' (m3/s)', xp, yp , klsrc)
          else if (ndraw(41) == 5 .and. isalt > 0) then
             if (qsrc(n) > 0d0) then
                write(tex,'(f10.3)') ccsrc(isalt,n)
                call gtext(trim(tex)//' (ppt)', xp, yp , klsrc)
             endif
          else if (ndraw(41) == 6 .and. itemp > 0) then
             if (qsrc(n) > 0d0) then
                write(tex,'(f10.3)') ccsrc(itemp,n)
                call gtext(trim(tex)//' (degC)', xp, yp , klsrc)
             endif
          endif
      endif
   endif
enddo

end subroutine teksorsin
