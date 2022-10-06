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

! $Id: copynetwtonetw.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/copynetwtonetw.f90 $

 subroutine copynetwtonetw()
 use M_MAPPROPARAMETERS
 use m_netw
 use m_missing
 use m_polygon, only: NPL, xpl, ypl, zpl
 use geometry_module, only: dbpinpol
 use gridoperations

 implicit none
 integer :: in, k, n, L, k0, L0, numkn, numLn, ja


 call savenet()

 call converparameters(ja)

 KC = 0 ; in = -1
 numkn = 0 ; numLn = 0
 do n  = 1,numk
    CALL DBPINPOL(XK(n), YK(n), IN, dmiss, JINS, NPL, xpl, ypl, zpl)
    IF (IN  == 1) THEN
       numkn = numkn + 1
       KC(n) = numkn
    ENDIF
 ENDDO

 do L = 1,numl
    if (kc(kn(1,L)) > 0 .and. kc(kn(2,L)) > 0) then
        numLn = numLn + 1
    endif
 enddo

 K0 = numk ; L0 = numL
 CALL INCREASENETW(K0+NUMKN, L0 + NUMLN)

 KC = 0 ; in = -1          ! redo kc after increasenetw
 numkn = 0 ; numLn = 0
 do n  = 1,numk
    CALL DBPINPOL(XK(n), YK(n), IN, dmiss, JINS, NPL, xpl, ypl, zpl)
    IF (IN  == 1) THEN
       numkn = numkn + 1
       KC(n) = numkn
    ENDIF
 ENDDO

 numLn = 0
 do L  = 1,numl
    if (kc(kn(1,L)) > 0 .and. kc(kn(2,L)) > 0) then
        numLn       = numLn + 1
        kn(1,numLn+L0) = kc( kn(1,L) ) + numk
        kn(2,numLn+L0) = kc( kn(2,L) ) + numk
        kn(3,numLn+L0) =     kn(3,L)
    endif
 enddo

 do n  = 1,numk
    IF (kc(n) > 0) THEN
       xk( kc(n) + numk ) = xk(n) + deltx
       yk( kc(n) + numk ) = yk(n) + delty
       zk( kc(n) + numk ) = zk(n)
    ENDIF
 ENDDO


 numL = L0 + numLn
 numk = K0 + NUMKN

 end subroutine copynetwtonetw
