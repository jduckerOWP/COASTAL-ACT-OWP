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
!  $Id: write_srf.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/utils_lgpl/io_hyd/packages/io_hyd/src/write_srf.f90 $

      subroutine write_srf ( file_srf, mmax  , nmax  , nosegl, surf  )
!
!     created             : jan van beek
!
!     function            : writes horizontal surface file.
!
!     subroutines called  : -
!                         : dlwq_platform, return platform type
!
      ! global declarations

      use filmod                   ! module contains everything for the files
      implicit none

      ! declaration of arguments

      type(t_dlwqfile)                       :: file_srf               ! surfaces-file
      integer                                :: mmax                   ! grid cells m direction
      integer                                :: nmax                   ! grid cells n direction
      integer                                :: nosegl                 ! number of segments per layer
      real                                   :: surf(nosegl)           ! surf

      ! local declarations

      integer       lun
      integer       i
      integer       idummy
      integer       irlen
      integer       plform
      integer       filtyp
      integer       filsta

      plform = dlwq_platform()
      idummy = 0

      ! initialise file

      call dlwqfile_open(file_srf)
      lun    = file_srf%unit_nr
      filtyp = file_srf%type

      ! write surfaces file

      if ( filtyp .eq. FT_UNF .or. filtyp .eq. FT_BIN) then
         write (lun) nmax,mmax,nosegl,nosegl,nosegl,idummy
         write (lun) (surf(i),i=1,nosegl)
      elseif ( filtyp .eq. FT_ASC ) then
         write (lun,'(4i8)') nmax,mmax,nosegl,nosegl,nosegl,idummy
         write (lun,'(e13.6)') (surf(i),i=1,nosegl)
      endif

      close(file_srf%unit_nr)
      file_srf%status = FILE_STAT_UNOPENED

      return
      end

      subroutine write_hsrf ( file_hsrf, noseg, surf  )
!
!     created             : michelle jeuken
!
!     function            : writes horizontal surface file (new unstructured style).
!
      ! global declarations

      use filmod                   ! module contains everything for the files
      implicit none

      ! declaration of arguments

      type(t_dlwqfile)                       :: file_hsrf              ! surfaces-file
      integer                                :: noseg                  ! number of segments
      real                                   :: surf(noseg)            ! horizontal surfaces

      ! local declarations

      integer       lun
      integer       i
      integer       idummy
      integer       irlen
      integer       plform
      integer       filtyp
      integer       filsta

      plform = dlwq_platform()
      idummy = 0

      ! initialise file

      call dlwqfile_open(file_hsrf)
      lun    = file_hsrf%unit_nr
      filtyp = file_hsrf%type

      ! write horizontal surfaces file
      if ( filtyp .eq. FT_UNF .or. filtyp .eq. FT_BIN) then
         write (lun) idummy, (surf(i),i=1,noseg)
      elseif ( filtyp .eq. FT_ASC ) then
         write (lun,'(i8)') idummy
         write (lun,'(e13.6)') (surf(i),i=1,noseg)
      endif

      close(file_hsrf%unit_nr)
      file_hsrf%status = FILE_STAT_UNOPENED

      return
      end
