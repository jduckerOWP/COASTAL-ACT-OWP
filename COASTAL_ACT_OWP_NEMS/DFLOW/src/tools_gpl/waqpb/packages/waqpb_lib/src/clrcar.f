!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2022.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: clrcar.f 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/tools_gpl/waqpb/packages/waqpb_lib/src/clrcar.f $

      subroutine clrcar (ndim, indarr, array )
      integer ndim
      integer indarr(ndim)
      character*(*) array(ndim)

c     Remove all elements for which the index = 0
c     Without actually updating the index array

      integer ndiml, idim

      ndiml = 0
      do 10 idim = 1,ndim
          if (indarr(idim) .gt. 0 ) then
              ndiml = ndiml + 1
              array(ndiml) = array(idim)
          endif
   10 continue

      return
      end

      subroutine clriar (ndim, indarr, array )
      integer ndim
      integer indarr(ndim)
      integer array(ndim)

c     Remove all elements for which the index = 0
c     Without actually updating the index array

      integer ndiml, idim

      ndiml = 0
      do 10 idim = 1,ndim
          if (indarr(idim) .gt. 0 ) then
              ndiml = ndiml + 1
              array(ndiml) = array(idim)
          endif
   10 continue

      return
      end

      subroutine clrrar (ndim, indarr, array )
      integer ndim
      integer indarr(ndim)
      real array(ndim)

c     Remove all elements for which the index = 0
c     Without actually updating the index array

      integer ndiml, idim

      ndiml = 0
      do 10 idim = 1,ndim
          if (indarr(idim) .gt. 0 ) then
              ndiml = ndiml + 1
              array(ndiml) = array(idim)
          endif
   10 continue

      return
      end

      subroutine updind (ndim, indarr)
      integer ndim
      integer indarr(ndim)

c     Remove all elements for which the index = 0
c     And update length of table!!

      integer ndiml, idim

      ndiml = 0
      do 10 idim = 1,ndim
          if (indarr(idim) .gt. 0 ) then
              ndiml = ndiml + 1
              indarr(ndiml) = indarr(idim)
          endif
   10 continue
      ndim = ndiml

      return
      end

