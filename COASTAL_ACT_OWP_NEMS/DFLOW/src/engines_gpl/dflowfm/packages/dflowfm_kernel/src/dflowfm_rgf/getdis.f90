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

! $Id: getdis.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_rgf/getdis.f90 $

      SUBROUTINE GETDIS(X,Y,X2,Y2,N,TS,SS,H)

      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none
!     Bereken de afstand SS van punt TS in X,Y, tov punt met TS = 0, ofwel N=1
      double precision ::  X(N), Y(N), X2(N), Y2(N)
      double precision :: ts, ss
      integer :: n
      double precision :: dt, t0, xt0, yt0, t1, xt1, yt1, dnx, dny, dsx, dsy

      double precision, intent(in) :: H  !< for curvature dependent meshing (>0) or disable (<=0)

      double precision :: curv
      logical          :: Lcurv

      Lcurv = ( H.gt.1d-8 )

      TS  = MIN(TS,dble(N))
      DT  = 0.1
      SS  = 0
      T0  = 0
      XT0 = X(1)
      YT0 = Y(1)
    10 CONTINUE
      T1  = T0 + DT
      IF (T1 .LT. TS) THEN
         CALL SPLINTXY(X,Y,X2,Y2,N,T1,XT1,YT1)
         if ( Lcurv ) call comp_curv(N,X,Y,X2,Y2,0.5d0*(T0+T1),curv,dnx,dny,dsx,dsy)
      ELSE
         CALL SPLINTXY(X,Y,X2,Y2,N,TS,XT1,YT1)
         if ( Lcurv ) call comp_curv(N,X,Y,X2,Y2,0.5d0*(T0+TS),curv,dnx,dny,dsx,dsy)
      ENDIF
      if ( .not.Lcurv ) then
!         SS  = SS + SQRT( (XT1-XT0)**2 + (YT1-YT0)**2 )
         SS  = SS + dbdistance(xt0,yt0,xt1,yt1,jsferic, jasfer3D, dmiss)
      else
         SS  = SS + dbdistance(xt0,yt0,xt1,yt1,jsferic, jasfer3D, dmiss)*(1d0+H*curv)
      end if

      T0  = T1
      XT0 = XT1
      YT0 = YT1
      IF (T1 .LT. TS) GOTO 10

      RETURN
      END
