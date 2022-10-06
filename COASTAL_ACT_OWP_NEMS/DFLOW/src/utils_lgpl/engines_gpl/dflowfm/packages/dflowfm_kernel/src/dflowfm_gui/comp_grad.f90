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

! $Id: comp_grad.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/comp_grad.f90 $

!>  compute the gradient in a control volume defined by the polygon (0-R-1-L)
!>
!>      0L-----1L
!>       |     |
!>       |  L  |
!>       | / \ |
!>       |/   \|
!>       0-----1
!>       |\   /|
!>       | \ / |
!>       |  R  |
!>       |     |
!>      0R-----1R
!>
!> 0 and 1 are sample points
!> L and R are interpolated at sample cell centers
   subroutine comp_grad(zss, ip0, ip1, ip0L, ip0R, ip1L, ip1R, gradx, grady, Sx, Sy, DareaL, DareaR)
      use m_samples, only: Ns, MXSAM, MYSAM, xs, ys
      use m_samples_refine, only: NDIM
      use m_missing
      use m_sferic
      use geometry_module, only: getdxdy, getdx, getdy, dprodout

      implicit none
      double precision, dimension(NDIM,MXSAM*MYSAM)     :: zss
      integer,                              intent(in)  :: ip0, ip1, ip0L, ip0R, ip1L, ip1R  !> node numbers
      double precision,                     intent(out) :: gradx, grady                      !> gradient vector components
      double precision,                     intent(out) :: Sx, Sy                            !> edge (nx,ny)dS vector (for divergence)
      double precision,                     intent(out) :: DareaL, DareaR                    !> contributions to control volume area (for divergence)

      double precision                                  :: x0, y0, z0, cx0, cy0
      double precision                                  :: x1, y1, z1, cx1, cy1
      double precision                                  :: xL, yL, zL, cxL, cyL
      double precision                                  :: xR, yR, zR, cxR, cyR
      double precision                                  :: darea


      gradx = DMISS
      grady = DMISS
      Sx    = 0d0
      Sy    = 0d0
      dareaL = 0d0
      dareaR = 0d0

      x0 = xs(ip0)
      y0 = ys(ip0)
      z0 = zss(1,ip0)

      x1 = xs(ip1)
      y1 = ys(ip1)
      z1 = zss(1,ip1)

      if ( x0.eq.DMISS .or. y1.eq.DMISS .or. x1.eq.DMISS .or. y1.eq.DMISS ) goto 1234

      xL = 0.25d0*(xs(ip0)+xs(ip1)+xs(ip0L)+xs(ip1L))
      yL = 0.25d0*(ys(ip0)+ys(ip1)+ys(ip0L)+ys(ip1L))
      zL = 0.25d0*(zss(1,ip0)+zss(1,ip1)+zss(1,ip0L)+zss(1,ip1L))

      xR = 0.25d0*(xs(ip0)+xs(ip1)+xs(ip0R)+xs(ip1R))
      yR = 0.25d0*(ys(ip0)+ys(ip1)+ys(ip0R)+ys(ip1R))
      zR = 0.25d0*(zss(1,ip0)+zss(1,ip1)+zss(1,ip0R)+zss(1,ip1R))


      call getdxdy(xL,yL,xR,yR,cy1,cx1, jsferic) ; cx1 = -cx1
      call getdxdy(x0,y0,x1,y1,cyL,cxL, jsferic) ; cxL = -cxL
      !cx1 = -0.5d0*getdy(xL,yL,xR,yR)
      !cy1 =  0.5d0*getdx(xL,yL,xR,yR)
      !cxL = -0.5d0*getdy(x0,y0,x1,y1)
      !cyL =  0.5d0*getdx(x0,y0,x1,y1)

      cx0 = -cx1
      cy0 = -cy1
      cxR = -cxL
      cyR = -cyL

      darea =  0.5d0*(cx0*x0+cy0*y0 + cx1*x1+cy1*y1 + cxL*xL+cyL*yL + cxR*xR+cyR*yR)

!     gradx and grady can be composed

      if ( zss(1,ip0) .ne.DMISS .and. zss(1,ip1) .ne.DMISS .and.   &
            zss(1,ip0L).ne.DMISS .and. zss(1,ip0R).ne.DMISS .and. &
            zss(1,ip1L).ne.DMISS .and. zss(1,ip1R).ne.DMISS ) then
         gradx = (cx1*z1 + cxL*zL + cx0*z0 + cxR*zR)/darea
         grady = (cy1*z1 + cyL*zL + cy0*z0 + cyR*zR)/darea
      end if

      Sx = 2d0*cx1
      Sy = 2d0*cy1
      DareaL = 0.5d0*abs(dprodout(x0,y0,xR,yR,x0,y0,xL,yL, jsferic, jasfer3D))
      DareaR = 0.5d0*abs(dprodout(x1,y1,xR,yR,x1,y1,xL,yL, jsferic, jasfer3D))

   1234 continue

      return
end subroutine comp_grad
