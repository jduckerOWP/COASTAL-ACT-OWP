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

! $Id: reabar2pli.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_io/reabar2pli.f90 $

 subroutine reabar2pli(mthd, mout)   ! convert barrier v file to model independent, barv content =  m,n,sill depth
 use m_grid
 implicit none
 integer :: mthd, mout
 double precision      :: xce, yce,dep
 character (len = 132) :: rec
 character (len = 1 )  :: uv
 integer               :: m,n,m2,n2, mn, mx, nn, nx, i

 10 read(mthd,'(a)', end = 999) rec

 read(rec,*) m,n,dep

 write(mout,'(a)') 'Line'
 write(mout,'(a)') ' 2 2'

 if ( index(rec,'u') > 0 .or. index(rec,'U') > 0 ) then

     write(mout,*) xc(m,n-1), yc(m,n-1) ,-dep
     write(mout,*) xc(m,n)  , yc(m,n)   ,-dep

 else

     write(mout,*) xc(m-1,n), yc(m-1,n) ,-dep
     write(mout,*) xc(m,n)  , yc(m,n)   ,-dep

 endif

 goto 10

 999 call doclose (mthd)
     call doclose (mout)

 end subroutine reabar2pli
