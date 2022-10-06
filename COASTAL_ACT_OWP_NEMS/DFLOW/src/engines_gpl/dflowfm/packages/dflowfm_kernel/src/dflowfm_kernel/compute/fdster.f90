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

! $Id: fdster.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/fdster.f90 $

 subroutine fdster(dster,taucr,thetcr,pclay,g,d50,rhos,rhow,FCR)
 IMPLICIT NONE
 double precision :: dster,taucr,thetcr,pclay,g,d50,rhos,rhow,FCR
 double precision :: dsand, dsilt,cmaxs,fch1,cmax,fpack,fclay
 IF(DSTER.LE.1.)THETCR=.24  ! this line added by hk and svdp: critical shields parameter
 IF(DSTER.LE.4.)THETCR=0.115/(DSTER)**0.5
 IF(4. .LT.DSTER.AND.DSTER.LE.10.)THETCR=.14*DSTER**(-.64)
 IF(10..LT.DSTER.AND.DSTER.LE.20.)THETCR=.04*DSTER**(-.1 )
 IF(20..LT.DSTER.AND.DSTER.LE.150.)THETCR=.013*DSTER**(.29 )
 IF(DSTER.GT.150.)THETCR=.055
!Soulsby gives one single formula
!THETCR=(0.24/DSTER)+0.055*(1.0-exp(-0.02*DSTER))
 dsand=0.000062
 dsilt=0.000032
 cmaxs=0.65
 fch1=(dsand/d50)**1.5
 cmax=(d50/dsand)*cmaxs
!cmaxs=maximum bed concentration in case of sandy bottom (=0.65)
 if(cmax.lt.0.05)cmax=0.05
 if(cmax.gt.cmaxs)cmax=cmaxs
 fpack=cmax/cmaxs
 if(fch1.lt.1.)fch1=1.
 if(fpack.gt.1.)fpack=1.
 fclay=1.
 if(pclay.ge.0.)fclay=(1.+Pclay)**3.
!  if(pclay.ge.0..and.d50.ge.dsand)fclay=(1.+Pclay)**3.
 if(fclay.ge.2.)fclay=2.

 thetcr=FCR*fpack*fch1*fclay*THETCR

 TAUCR=(RHOS-RHOW)*G*D50*THETCR
 end
