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
!  $Id: copy_hyd_step.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/utils_lgpl/io_hyd/packages/io_hyd/src/copy_hyd_step.f90 $

      subroutine copy_hyd_step(input_hyd, output_hyd)

      ! function : copy one hydrodynamic step

      ! global declarations

      use hydmod
      implicit none

      ! declaration of the arguments

      type(t_hyd)         :: input_hyd     ! the input hydrodynamics
      type(t_hyd)         :: output_hyd    ! the output hydrodynamics

      ! local declarations

      integer             :: iseg          ! segment index
      integer             :: iq            ! exchange index

      ! volumes
      do iseg = 1, input_hyd%noseg
         output_hyd%volume(iseg) = input_hyd%volume(iseg)
      end do


      ! areas

      do iq = 1 , input_hyd%noq
         output_hyd%area(iq) = input_hyd%area(iq)
      enddo

      ! flows

      do iq = 1 , input_hyd%noq
         output_hyd%flow(iq) = input_hyd%flow(iq)
      enddo

      ! salinity

      if ( input_hyd%sal_present ) then
         do iseg = 1, input_hyd%noseg
            output_hyd%sal(iseg) = input_hyd%sal(iseg)
         end do
      endif

      ! temperature, averaged with volume

      if ( input_hyd%tem_present ) then
         do iseg = 1, input_hyd%noseg
            output_hyd%tem(iseg) = input_hyd%tem(iseg)
         end do
      endif

      ! tau

      if ( input_hyd%tau_present ) then
         do iseg = 1, input_hyd%noseg
            output_hyd%tau(iseg) = input_hyd%tau(iseg)
         end do
      endif

      ! vdf

      if ( input_hyd%vdf_present ) then
         do iseg = 1, input_hyd%noseg
            output_hyd%vdf(iseg) = input_hyd%vdf(iseg)
         end do
      endif

      return
      end
