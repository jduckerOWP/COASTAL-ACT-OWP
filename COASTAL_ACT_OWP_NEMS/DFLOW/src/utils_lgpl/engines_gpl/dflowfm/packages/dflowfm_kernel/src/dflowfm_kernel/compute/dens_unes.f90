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

! $Id: dens_unes.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/dens_unes.f90 $

 subroutine dens_unes(temp, salt, rhouns, rhods, rhodt)
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
!  $Id: dens_unes.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/dens_unes.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes water density from temperature and
!              salinity using equation of state (rhowat).
!
! Method used: Equation of state following UNESCO, (UNESCO,
!              Algorithms for computation of fundamental
!              properties of seawater, UNESCO technical papers
!              in marine science, 1983)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
    implicit none
!
! Global variables
!
    double precision, intent(in)     :: salt, temp
    double precision, intent(out)    :: rhouns, rhods, rhodt
!
! Local variables
!
    double precision :: s, sq, rhwa, asal, bsal
    double precision, dimension(5) :: t
    double precision, dimension(0:5), parameter :: cf = &
      (/ 999.842594d0   ,&
         6.793952d-2  ,&
        -9.095290d-3  ,&
         1.001685d-4  ,&
        -1.120083d-6  ,&
         6.536332d-9  /)
    double precision, dimension(0:4), parameter :: ca = &
    (/   8.24493d-1 ,&
        -4.0899d-3  ,&
         7.6438d-5  ,&
        -8.2467d-7  ,&
         5.3875d-9  /)
    double precision, dimension(0:2), parameter :: cb = &
    (/  -5.72466d-3  ,&
         1.0227d-4   ,&
        -1.6546d-6   /)
    double precision,                 parameter :: csal = 4.8314d-4
!
!! executable statements -------------------------------------------------------
!
    t(1)   = temp
    t(2)   = temp*t(1)
    t(3)   = temp*t(2)
    t(4)   = temp*t(3)
    t(5)   = temp*t(4)
    !
    s      = salt
    sq     = sqrt(max(0d0,s))
    !
    rhwa   = cf(0) + cf(1)*t(1) + cf(2)*t(2) + cf(3)*t(3) + cf(4)*t(4) &
           &       + cf(5)*t(5)
    asal   = ca(0) + ca(1)*t(1) + ca(2)*t(2) + ca(3)*t(3) + ca(4)*t(4)
    bsal   = cb(0) + cb(1)*t(1) + cb(2)*t(2)
    !
    rhouns = rhwa + (asal+bsal*sq+csal*s) * s
    return
    !
    rhods  = asal + 1.5d0*bsal*sq + 2.0d0*csal*s
    !
    rhodt  = cf(1) +  2.0d0*cf(2)*t(1) + 3.0d0*cf(3)*t(2) &
           &       +  4.0d0*cf(4)*t(3) + 5.0d0*cf(5)*t(4) &
           &       + (ca(1) + 2.0d0*ca(2)*t(1) + 3.0d0*ca(3)*t(2) &
           &       +  4.0d0*ca(4)*t(3)) * s &
           &       + (cb(1) + 2.0d0*cb(2)*t(1)) * sq * s
end subroutine dens_unes
