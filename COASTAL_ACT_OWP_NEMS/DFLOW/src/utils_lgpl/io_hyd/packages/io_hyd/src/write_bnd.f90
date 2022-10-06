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
!  $Id: write_bnd.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/utils_lgpl/io_hyd/packages/io_hyd/src/write_bnd.f90 $

      subroutine write_bnd(hyd)

      ! function : write the attributes file

      ! global declarations

      use hydmod                   ! module contains everything for the hydrodynamics
      implicit none

      ! declaration of the arguments

      type(t_hyd)                            :: hyd                    ! description of the hydrodynamics

      ! local declarations

      integer                                :: lunbnd                 ! unit number bnd file
      integer                                :: no_sect                ! number of sections
      integer                                :: i_sect                 ! index of section
      integer                                :: no_bnd                 ! number of boundaries in section
      integer                                :: i_bnd                  ! index of boundary
      integer                                :: ibnd                   ! index of boundary
      real*8                                 :: x1                     ! x1
      real*8                                 :: y1                     ! y1
      real*8                                 :: x2                     ! x2
      real*8                                 :: y2                     ! y2
      type(t_openbndsect), pointer           :: openbndsect            ! single section read
      type(t_openbndlin), pointer            :: openbndlin             ! single open boundary lin

      call dlwqfile_open(hyd%file_bnd)
      lunbnd = hyd%file_bnd%unit_nr

      no_sect = hyd%openbndsect_coll%cursize
      write(lunbnd, '(i8)') no_sect                   ! Nr of open boundary sections.

      do i_sect = 1, no_sect

          openbndsect => hyd%openbndsect_coll%openbndsect_pnts(i_sect)
          no_bnd = openbndsect%openbndlin_coll%cursize

          write(lunbnd, '(a)')  trim(openbndsect%name)              ! Section name
          write(lunbnd, '(i8)') no_bnd                              ! Nr of lins in section

          do i_bnd = 1, no_bnd
              openbndlin => openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd)
              ibnd = openbndlin%ibnd
              x1   = openbndlin%x1
              y1   = openbndlin%y1
              x2   = openbndlin%x2
              y2   = openbndlin%y2
              write(lunbnd, '(i8,4f18.8)') ibnd, x1, y1, x2, y2
          enddo
      enddo

      return
      end
