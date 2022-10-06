module precision
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
!  $Id: precision.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/tools_gpl/lint/packages/lint_f/src/precision.f90 $
!!--description-----------------------------------------------------------------
!
! This module contains the parameters used to switch easily from
! single precision mode to double precision mode.
!
!
! See also precision.h file for C-code (DD)
! See also tri-dyn.igd file for connection with esm
!
! sp: single precision
! hp: high (or double) precision
! fp: flexible precision, single or double
!     fp is the used precision
!
! SWITCHING FROM SINGLE PRECISION   FP   TO DOUBLE PRECISION:
! 1) File libsrc\flow_modsrc\precision.f90
!    - Comment out the following line:
!      integer, parameter :: fp=sp
!    - Activate the following line:
!      integer, parameter :: fp=hp
! 2) File include\flow\tri-dyn.igd
!    - Comment out the following line:
!      equivalence ( r(0),  rbuf(0))
!    - Activate the following line:
!      equivalence ( r(0),  dbuf(0))
! 3) File include\hydra\precision.h
!    - Comment out the following line:
!      #undef FLOW_DOUBLE_PRECISION
!    - Activate the following line:
!      #define FLOW_DOUBLE_PRECISION
!
! SWITCHING FROM SINGLE PRECISION BODSED/DPS TO DOUBLE PRECISION:
! 1) File libsrc\flow_modsrc\precision.f90
!    - Comment out the following line:
!      integer, parameter :: prec=sp
!    - Activate the following line:
!      integer, parameter :: prec=hp
! 2) File include\flow\tri-dyn.igd
!    - Comment out the following line:
!      equivalence ( d(0),  rbuf(0))
!    - Activate the following line:
!      equivalence ( d(0),  dbuf(0))
! 3) File libsrc\flow_dd\hyexth\precision.h
!    - Comment out the following line:
!      #undef PREC_DOUBLE_PRECISION
!    - Activate the following line:
!      #define PREC_DOUBLE_PRECISION
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
implicit none
!
! parameters, used in conversions: sp=single precision, hp=high (double) precision
!
integer, parameter :: sp=kind(1.0e00)
integer, parameter :: hp=kind(1.0d00)
!
! double precision integers:
!
integer, parameter :: long = SELECTED_INT_KIND(16)
!
! fp is the generally used precision in Delft3D-FLOW
!
integer, parameter :: fp=hp
!integer, parameter :: fp=sp
!
! prec is used to switch bodsed/dps from sp to hp
!
integer, parameter :: prec=hp
!integer, parameter :: prec=sp
end module precision
