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

! $Id: bilin6.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/rest_f90/bilin6.f90 $

!>    bilineair interpolation between four nodes
      subroutine bilin6(x, y, z, xp, yp, zp)
      use m_missing

      implicit none

      double precision, dimension(2,2), intent(in)  :: x, y   !< node coordinates
      double precision, dimension(2,2), intent(in)  :: z      !< node values
      double precision,                 intent(in)  :: xp, yp !< interpolant coordinates
      double precision,                 intent(out) :: zp     !< interpolant value

      integer                                       :: ierror

      double precision                              :: A, B, C, D
      double precision                              :: E, F, G, H
      double precision                              :: P, Q, R, S
      double precision                              :: xi1, eta1, xp1, yp1
      double precision                              :: xi2, eta2, xp2, yp2

      double precision                              :: aa, bb, cc, dis

      double precision, parameter                   :: dtol = 1d-9

      ierror = 1

      zp = DMISS

!     the interpolant has the form:
!        xp(xi,eta) = A + B xi + C eta + D xi eta
!        yp(xi,eta) = E + F xi + G eta + H xi eta
!        zp(xi,eta) = P + Q xi + R eta + S xi eta

      A = x(1,1)
      B = x(2,1) - x(1,1)                       ! "xi-derivative"
      C = x(1,2) - x(1,1)                       ! "eta-derivative"
      D = (x(2,2) - x(2,1)) - (x(1,2)-x(1,1))   ! "xi-eta-derivative"

      E = y(1,1)
      F = y(2,1) - y(1,1)                       ! "xi-derivative"
      G = y(1,2) - y(1,1)                       ! "eta-derivative"
      H = (y(2,2) - y(2,1)) - (y(1,2)-y(1,1))   ! "xi-eta-derivative"

      P = z(1,1)
      Q = z(2,1) - z(1,1)                       ! "xi-derivative"
      R = z(1,2) - z(1,1)                       ! "eta-derivative"
      S = (z(2,2) - z(2,1)) - (z(1,2)-z(1,1))   ! "xi-eta-derivative"

!     determine xi and eta from xp(xi,eta) = xp and yp(xi,eta)=yp
!        aa xi^2 + bb xi + cc = 0
      aa = D*F - B*H
      bb = H*(xp-A) - B*G - D*(yp-E) + C*F
      cc = G*(xp-A) - C*(yp-E)

      dis = bb*bb - 4.0d0*aa*cc

      if ( dis.lt.0d0 ) goto 1234

      if ( abs(aa).gt.dtol ) then
         dis = sqrt(dis)
         aa  = 0.5d0 / aa
         xi1 = (-bb - dis) * aa
         xi2 = (-bb + dis) * aa
      else if ( abs(bb).gt.1d-9) then ! bb*xi + cc = 0
         xi1 = -cc/bb
         xi2 = xi1
      else  ! no or infinitely many solutions
         goto 1234
      end if

      eta1 = C+D*xi1
      if ( abs(eta1).gt.dtol ) then
         eta1 = (xp-(A+B*xi1))/eta1
      else
         eta1 = G + H*xi2
         if ( abs(eta1).gt.dtol ) then
            eta1 = (yp-E-F*xi1)/ eta1
         else
            eta1 = DMISS
         end if
      end if

      eta2 = C+D*xi2
      if ( abs(eta2).gt.dtol ) then
         eta2 = (xp-A-B*xi2)/eta2
      else
         eta2 = G + H*xi2
         if ( abs(eta2).gt.dtol ) then
            eta2 = (yp-E-F*xi2)/ eta2
         else
            eta2 = DMISS
         end if
      end if

!     determine zp
      do
!        try first (xi,eta)
         if ( xi1.ne.DMISS .and. eta1.ne.DMISS ) then
            xp1 = A + B*xi1 + C*eta1 + D*xi1*eta1
            yp1 = E + F*xi1 + G*eta1 + H*xi1*eta1
            if ( abs(xp1-xp).lt.dtol .and. abs(yp1-yp).lt.dtol ) then
               zp = P + Q*xi1 + R*eta1 + S*xi1*eta1
               exit
            end if
         end if

!        try second (xi,eta)
         if ( xi2.ne.DMISS .and. eta2.ne.DMISS ) then
            xp2 = A + B*xi2 + C*eta2 + D*xi2*eta2
            yp2 = E + F*xi2 + G*eta2 + H*xi2*eta2
            if ( abs(xp1-xp).lt.dtol .and. abs(yp1-yp).lt.dtol ) then
               zp = P + Q*xi2 + R*eta2 + S*xi2*eta2
               exit
            end if
         end if

!        no solution found
         goto 1234
      end do

      ierror = 0

!     error handling
 1234 continue

      return
      end
