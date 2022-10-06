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

! $Id: a1x1a2x2.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/rest_f90/a1x1a2x2.f90 $

! compute coordinates (xu, yu) from (x1,y1) and (x2,y2) with
!    weights alpha1 and alpha2
subroutine a1x1a2x2(x1,y1,x2,y2,alpha1,alpha2,xu,yu)
   use m_sferic
   use geometry_module, only: sphertocart3D, Cart3Dtospher
   implicit none

   double precision, intent(in)  :: x1, y1
   double precision, intent(in)  :: x2, y2
   double precision, intent(in)  :: alpha1
   double precision, intent(in)  :: alpha2
   double precision, intent(out) :: xu, yu

   double precision              :: xx1, yy1, zz1, xx2, yy2, zz2
   double precision              :: xxu, yyu, zzu

   if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
      call sphertoCart3D(x1,y1,xx1,yy1,zz1)
      call sphertoCart3D(x2,y2,xx2,yy2,zz2)
      xxu = alpha1*xx1 + alpha2*xx2
      yyu = alpha1*yy1 + alpha2*yy2
      zzu = alpha1*zz1 + alpha2*zz2
      call Cart3Dtospher(xxu,yyu,zzu,xu,yu,max(x1,x2))
   else
      xu = alpha1*x1 + alpha2*x2
      yu = alpha1*y1 + alpha2*y2
   end if

   return
end subroutine a1x1a2x2
