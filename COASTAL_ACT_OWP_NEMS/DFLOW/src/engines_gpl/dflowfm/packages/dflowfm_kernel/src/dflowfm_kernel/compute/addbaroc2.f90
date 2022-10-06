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

! $Id: addbaroc2.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/addbaroc2.f90 $

 subroutine addbaroc2(LL,Lb,Lt)
 use m_flowgeom
 use m_flow

 implicit none
 integer              :: LL,Lb,Lt

 double precision     :: gradpu(kmxx), rhovol(kmxx), dz1(kmxx), dz2(kmxx)
 double precision     :: z1u,z1d,z2u,z2d,  p1u,p1d,p2u,p2d,  r1u,r1d,r2u,r2d,  dz3, d2
 double precision     :: barotr, barocl, alf1,alf2,alf3,gr1,gr2,gr3, zh, hdx, fzu1, fzd1, fzu2, fzd2, dzz, dxx, rvd, rv, vv, rvv, r
 integer              :: k1, k2, L

 ! rho = rhomean
 ! do L = Lb, Lt
 !    k1 = ln(1,L)
 !    rho(k1) = rhomean + 0.5d0*(zws(k1) + zws(k1-1))
 !    k2 = ln(2,L)
 !    rho(k2) = rhomean + 0.5d0*(zws(k2) + zws(k2-1))
 ! enddo

 if (kmx == 0) then
    k1 = ln(1,LL) ; k2 = ln(2,LL)
    barocl   = ag*( rho(k1) - rho(k2) )*hu(LL)*dxi(LL) / ( ( rho(k2) + rho(k1) ) )
    if (jarhoxu > 0) then
       rhou(LL) =  0.5d0*( rho(k2) + rho(k1) )
    endif
    adve(LL) = adve(LL) - barocL
    return
 endif

 if (zws(ln(1,Lt)) - zws(ln(1,Lb)-1) < epshs) return

 if (zws(ln(2,Lt)) - zws(ln(2,Lb)-1) < epshs) return

 do L = Lb, Lt
    k1 = ln(1,L)
    dz1(L-Lb+1) = max(1d-6, zws(k1) - zws(k1-1) )
 enddo

 do L = Lb, Lt
    k2 = ln(2,L)
    dz2(L-Lb+1) = max(1d-6, zws(k2) - zws(k2-1) )
 enddo

 k1   = ln(1,Lt)   ; k2  = ln(2,Lt)

 if (Lt > Lb) then
    d2   = (dz1(Lt-Lb+1) + dz1(Lt-Lb) )
    fzu1 = dz1(Lt-Lb+1) /  d2 ; fzd1 = 1d0 - fzu1

    d2   = (dz2(Lt-Lb+1) + dz2(Lt-Lb) )
    fzu2 = dz2(Lt-Lb+1) /  d2 ; fzd2 = 1d0 - fzu2

    if (jabaroctimeint == 2) then ! extrapolate rho at n+0.5
       r1d  = (2d0-fzu1)*(1.5d0*rho(k1) - 0.5d0*rho0(k1)) - fzd1*(1.5d0*rho(k1-1) - 0.5d0*rho0(k1-1)) - rhomean
       r2d  = (2d0-fzu2)*(1.5d0*rho(k2) - 0.5d0*rho0(k2)) - fzd2*(1.5d0*rho(k2-1) - 0.5d0*rho0(k2-1)) - rhomean
    else
       r1d  = (2d0-fzu1)*rho(k1) - fzd1*rho(k1-1) - rhomean
       r2d  = (2d0-fzu2)*rho(k2) - fzd2*rho(k2-1) - rhomean
    endif

    ! r1d = 1.5d0*rho(k1) - 0.5d0*rho(k1-1) - rhomean
    ! r2d = 1.5d0*rho(k2) - 0.5d0*rho(k2-1) - rhomean

 else
    if (jabaroctimeint == 2 ) then ! extrapolate rho at n+0.5
       r1d  = 1.5d0*rho(k1) - 0.5d0*rho0(k1) - rhomean
       r2d  = 1.5d0*rho(k2) - 0.5d0*rho0(k2) - rhomean
     else
       r1d  = rho(k1) - rhomean
       r2d  = rho(k2) - rhomean
     endif
 endif

 z1d = zws(k1)    ; z2d = zws(k2)
 p1d = 0d0        ; p2d = 0d0

 ! barotr = ag*(z2d - z1d) / dx(LL)
 ! r1d = abs(z1d) ; r2d = abs(z2d)  ! forced horizontal

 gradpu(1:Lt-Lb+1) = 0d0
 rhovol(1:Lt-Lb+1) = 0d0
 dxx               = dx(LL)
 hdx               = 0.5d0*dxx

 if (jabarocterm == 3) then
    gr3 = 0d0
    if (z1d > z2d) then              ! baroclinic contribution at free surface
       dz3  = z1d - z2d
       alf3 = r2d - r1d
       gr3  = p1d*dz3 + 0.5d0*r1d*dz3*dz3 + alf3*dz3*dz3/6d0
    else if (z1d < z2d) then
       dz3  = z2d - z1d
       alf3 = r1d - r2d
       gr3  = p2d*dz3 + 0.5d0*r2d*dz3*dz3 + alf3*dz3*dz3/6d0
       gr3  = -gr3
    endif
    gradpu(Lt-Lb+1) = gradpu(Lt-Lb+1) - gr3
 endif

 rvd  = 0d0
 do L = Lt, Lb, -1
    k1   = ln(1,L)   ; k2  = ln(2,L)            !
    z1u  = z1d       ; z2u = z2d
    r1u  = r1d       ; r2u = r2d
    p1u  = p1d       ; p2u = p2d

    z1d  = zws(k1-1) ; z2d = zws(k2-1)

    if (L > Lb) then
        fzu1 = dz1(L-Lb+1) / (dz1(L-Lb+1) + dz1(L-Lb) )   ; fzd1 = 1d0 - fzu1
        fzu2 = dz2(L-Lb+1) / (dz2(L-Lb+1) + dz2(L-Lb) )   ; fzd2 = 1d0 - fzu2
        if (jabaroctimeint == 2 ) then ! extrapolate rho at n+0.5
           r1d  = fzu1*(1.5d0*rho(k1)-0.5d0*rho0(k1)) + fzd1*(1.5d0*rho(k1-1)-0.5d0*rho0(k1-1)) - rhomean
           r2d  = fzu2*(1.5d0*rho(k2)-0.5d0*rho0(k2)) + fzd2*(1.5d0*rho(k2-1)-0.5d0*rho0(k2-1)) - rhomean
        else
           r1d  = fzu1*rho(k1) + fzd1*rho(k1-1) - rhomean
           r2d  = fzu2*rho(k2) + fzd2*rho(k2-1) - rhomean
        endif
    else
        if (Lt > Lb) then
           fzu1 = dz1(L-Lb+2) / (dz1(L-Lb+1) + dz1(L-Lb+2) ) ; fzd1 = 1d0 - fzu1
           fzu2 = dz2(L-Lb+2) / (dz2(L-Lb+1) + dz2(L-Lb+2) ) ; fzd2 = 1d0 - fzu2
           if (jabaroctimeint == 2 ) then ! extrapolate rho at n+0.5
              r1d  = (2d0-fzd1)*(1.5d0*rho(k1)-0.5d0*rho0(k1))  - fzu1*(1.5d0*rho(k1+1)-0.5d0*rho0(k1+1)) - rhomean
              r2d  = (2d0-fzd2)*(1.5d0*rho(k2)-0.5d0*rho0(k2))  - fzu2*(1.5d0*rho(k2+1)-0.5d0*rho0(k2+1)) - rhomean
           else
              r1d  = (2d0-fzd1)*rho(k1) - fzu1*rho(k1+1) - rhomean
              r2d  = (2d0-fzd2)*rho(k2) - fzu2*rho(k2+1) - rhomean
           endif
        else
           if (jabaroctimeint == 2 ) then ! extrapolate rho at n+0.5
              r1d  = 1.5d0*rho(k1) -0.5d0*rho0(k1) - rhomean
              r2d  = 1.5d0*rho(k2) -0.5d0*rho0(k2) - rhomean
           else
              r1d  = rho(k1) - rhomean
              r2d  = rho(k2) - rhomean
           endif
        endif
    endif

    if (dz1(L-Lb+1) + dz2(L-Lb+1) < 1d-10) then
       rhovol(L-Lb+1) = 1d-10 ; cycle
    else
       rhovol(L-Lb+1) = rhovol(L-Lb+1)    + dz1(L-Lb+1) *( rhomean+0.5d0*(r1u+r1d) )*hdx     ! left  interface Mass
       rhovol(L-Lb+1) = rhovol(L-Lb+1)    + dz2(L-Lb+1) *( rhomean+0.5d0*(r2u+r2d) )*hdx     ! right interface
       if (jarhoxu > 0) rhou(L) = rhovol(L-Lb+1) / ( (dz1(L-Lb+1)+dz2(L-Lb+1))*hdx )
    endif

    dzz  = dz1(L-Lb+1)
    alf1 = r1d - r1u
    p1d  = p1u + r1u*dzz + 0.5d0*alf1*dzz
    gr1  = p1u*dzz + 0.5d0*r1u*dzz*dzz + alf1*dzz*dzz/6d0             ! your left  wall

    dzz  = dz2(L-Lb+1)
    alf2 = r2d - r2u
    p2d  = p2u + r2u*dzz + 0.5d0*alf2*dzz                             ! alf2 ipv alf1
    gr2  = p2u*dzz + 0.5d0*r2u*dzz*dzz + alf2*dzz*dzz/6d0             ! your right wall

    if (jabarocterm == 3) then
       gr3 = 0d0 ; dz3 = 0d0
       if (z1d > z2d) then
          dz3  = z1d - z2d
          alf3 = r2d - r1d
          gr3  = p1d*dz3 + 0.5d0*r1d*dz3*dz3 + alf3*dz3*dz3/6d0          ! your own floor
       else if (z1d < z2d) then
          dz3  = z2d - z1d
          alf3 = r1d - r2d
          gr3  = p2d*dz3 + 0.5d0*r2d*dz3*dz3 + alf3*dz3*dz3/6d0          ! your own floor (kg/m)
          gr3  = -gr3
       endif
    else
       vv  = 0.5d0*( dz1(L-Lb+1) + dz2(L-Lb+1) )                         ! integrate rho volume
       r   = 0.25d0*( r1u + r1d + r2u + r2d )
       rvv = r*vv
       rvd = rvd + rvv
       gr3 = rvd*(z1d - z2d)
    endif


       gradpu(L-Lb+1) = gradpu(L-Lb+1)   + gr1 - gr2 + gr3
    if (L > Lb ) then
       gradpu(L-Lb)   = gradpu(L-Lb)                 - gr3            ! ceiling of ff# downstairs neighbours
    endif

 enddo

 if (jabaroctimeint == 3) then                                           ! original AB implementation

    do L = Lb, Lt
       if (rhovol(L-Lb+1) > 0d0) then
           barocl    = ag*gradpu(L-Lb+1)/rhovol(L-Lb+1)                  !
           adve(L)   = adve(L) - 1.5d0*barocl + 0.5d0*dpbdx0(L)
           dpbdx0(L) = barocL
        endif
    enddo

 else if (abs(jabaroctimeint) == 4) then                                      ! AB + better drying flooding

    do L = Lb, Lt
       if (rhovol(L-Lb+1) > 0d0) then
           barocl  = ag*gradpu(L-Lb+1)/rhovol(L-Lb+1)
           if (dpbdx0(L) .ne. 0d0) then
               adve(L)   = adve(L) - 1.5d0*barocl + 0.5d0*dpbdx0(L)
           else
               adve(L)   = adve(L) - barocl
           endif
           dpbdx0(L) = barocL
       endif
    enddo

    do L = Lt+1,Lb+kmxL(LL)-1
       dpbdx0(L) = 0d0
    enddo

 else

    do L = Lb, Lt
        if (rhovol(L-Lb+1) > 0d0) then
           barocl  = ag*gradpu(L-Lb+1)/rhovol(L-Lb+1)                     !  Explicit
           adve(L)   = adve(L) - barocl
       endif
    enddo

 endif

 end subroutine addbaroc2
