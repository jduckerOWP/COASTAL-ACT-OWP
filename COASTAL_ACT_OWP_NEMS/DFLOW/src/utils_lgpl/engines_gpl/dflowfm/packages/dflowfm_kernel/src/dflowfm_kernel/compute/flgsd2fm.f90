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

! $Id: flgsd2fm.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/flgsd2fm.f90 $

subroutine flgsd2fm(wsd, wstr, zs, w2, zb2, dg, ds1, ds2, elu, hd, rhoast,    &
                & cgd, imag, ds, lambda)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Local parameters
!
    double precision, parameter :: c23 = 2.0D0/3.0D0, c13 = 1.0D0/3.0D0
!
! Global variables
!
    logical, intent(out)           :: imag
    double precision, intent(in)   :: cgd
    double precision, intent(in)   :: dg
    double precision, intent(out)  :: ds
    double precision, intent(in)   :: ds1
    double precision, intent(in)   :: ds2
    double precision, intent(in)   :: elu
    double precision, intent(in)   :: hd
    double precision, intent(in)   :: lambda
    double precision, intent(in)   :: rhoast
    double precision, intent(in)   :: w2
    double precision, intent(in)   :: wsd
    double precision, intent(in)   :: wstr
    double precision, intent(in)   :: zb2
    double precision, intent(in)   :: zs
!
!
! Local variables
!
    double precision               :: ag
    double precision               :: bg
    double precision               :: cg
    double precision               :: d2
    double precision               :: det
    double precision               :: hsl
    double precision               :: terma
    double precision               :: termb
!
!
!! executable statements -------------------------------------------------------
!
    !
    !=======================================================================
    !                      Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:         J.Brouwer/J.Kuipers
    !
    ! Module:             FLGSD2 (FLow Gen. Struct. Depth sill 2nd ord. eq.)
    !
    ! Module description: Compute water depth ds at the sill by a second
    !                     order algebraic equation.
    !
    !                     In case of drowned gate flow the water level at
    !                     the sill is required. The water depth is calcu-
    !                     lated in this routine.
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    ! 12 cgd               I  Correction coefficient for drowned gate flow.
    !  6 dg                I  Gate opening height.
    !  7 ds1               I  Delta s1 general structure.
    !  8 ds2               I  Delta s2 general structure.
    ! 14 ds                IO Water level immediately downstream the gate.
    !  9 elu               I  Upstream energy level.
    ! 10 hd                I  Downstream water level.
    ! 13 imag              O  Logical indicator, = TRUE when determinant of
    !                         second order algebraic equation less than
    !                         zero.
    ! 15 lambda            I  Extra resistance in general structure.
    ! 11 rhoast            I  Downstream water density divided by upstream
    !                         water density.
    !  4 w2                I  Width at right side of structure.
    !  1 wsd               I  Width structure right or left side.
    !  2 wstr              I  Width at centre of structure.
    !  5 zb2               I  Bed level at right side of structure.
    !  3 zs                I  Bed level at centre of structure.
    !=======================================================================
    !
    !     Declaration of parameters:
    !
    !
    !     Declaration of local variables:
    !
    !
    !JK   LOGICAL uitput
    !JK   COMMON /UITPUT/uitput
    !
    !     Calculate Ag, Bg and Cg according to appendix C of
    !     the design document River Rural integratietraject deel 3.
    !JK   WRITE  (11,*)  'IN FLGSD2 ----'
    !
    ag = (1.0D0 - rhoast)*(w2/12.0D0 + wsd/4.0D0) + 0.5D0*(rhoast + 1.0D0)      &
       & *(c13*w2 + c23*wsd)
    d2 = hd - zb2
    !
    terma = (4.0D0*rhoast*cgd*cgd*dg*dg*wstr*wstr)/(w2*d2)*(1.0D0 + lambda/d2)
    termb = 4.0D0*cgd*dg*wstr
    !
    bg = (1.0D0 - rhoast)*((d2 + ds1)*(w2 + wsd)/6.D0 + ds1*wsd*c13)            &
       & + 0.5D0*(rhoast + 1.0D0)                                               &
       & *((ds1 + ds2 - d2)*(c13*w2 + c23*wsd) + (c23*d2 + c13*ds1)             &
       & *w2 + (c13*d2 + c23*ds1)*wsd) + terma - termb
    !
    hsl = elu - zs
    !
    cg = (1.0D0 - rhoast)*((d2 + ds1)**2*(w2 + wsd)/12.D0 + ds1**2*wsd/6.0D0)   &
       & + 0.5D0*(rhoast + 1.0D0)*(ds1 + ds2 - d2)                              &
       & *((c23*d2 + c13*ds1)*w2 + (c13*d2 + c23*ds1)*wsd) - terma*hsl +        &
       & termb*hsl
    !
    det = bg*bg - 4.0D0*ag*cg
    if (det<0.0D0) then
       imag = .true.
    !JK      WRITE (11,*) 'Det=',det
    else
       imag = .false.
       ds = ( - bg + sqrt(det))/(2.0D0*ag)
    endif
end subroutine flgsd2fm
