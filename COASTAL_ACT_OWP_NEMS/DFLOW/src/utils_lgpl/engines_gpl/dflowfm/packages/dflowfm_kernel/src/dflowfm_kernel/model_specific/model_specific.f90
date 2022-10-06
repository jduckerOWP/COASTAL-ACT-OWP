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

! $Id: model_specific.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/model_specific/model_specific.f90 $
!> @file model_specific.f90
!! A set of predefined routines that may contain model-specific actions.
!! Selection is based on global md_ident, and subroutines are automatically
!! called from main program routines (mainly unstruc.f90).
!!
!!
!! subroutine flowinit_model_specific AvD: TODO
!! subroutine textflow_model_specific
!! subroutine tekrai_model_specific AvD: TODO
!! subroutine externaloutput_model_specific AvD: TODO
!<

!> Print model-specific text strings on screen, based on current solution state.
subroutine textflowspecific()
    use unstruc_model, only: md_ident
    use m_flowgeom
    use network_data
    use m_flow
    use m_flowtimes
    use unstruc_colors
    use m_equatorial, only : ampliforced, amplifreeL, amplitotal, ndxforced, ndxfreeL, ndtforced, ndtfreeL, cflforced, cflfreeL, tforce, tfreeL, amplicomp
    use m_statistics
    use m_monitoring_crosssections
    implicit none

    double precision, external :: znod

    character(len=140) :: tex
    double precision :: solrest, eefrac, QCURV, QRECT
    integer :: nn !< Which horizontal node to print
    nn  = min(nplot,ndx)
    
    CALL SETTEXTSIZEFAC(2D0)

    if ( index(md_ident,'bend') > 0 .and. size(crs) > 0 ) then
        QCURV = crs(1)%sumvalcur(IPNT_Q1C)
        QRECT = crs(2)%sumvalcur(IPNT_Q1C)
        TEX =  ' '
        WRITE (TEX,'(A,F10.2,A,F10.2,A)')  'Q_Crs1 :', QCURV, ' (M3/S)         Q_Crs2 :', QRECT, ' (M3/S)'
        CALL ICTEXT(TRIM(TEX),6,14,221)
    endif

    IF ( md_ident == 'chezy') THEN
        TEX =  ' ' ; ! u=C*sqrt(hi): h=10, i=10-4,  u=C*0.03162277

        WRITE (TEX,'(A)')  'U_Chezy(60.) = 1.8974 m/s, U_Mann(.025) = 1.8566 m/s, U_WC(.05) = 1.9240 m/s'
        CALL ICTEXT(TRIM(TEX),6,24,ncolana)

        TEX = 'U_computed   = '
        WRITE (TEX(16:),'(f7.4)')  znod(nn)
        CALL ICTEXT(TRIM(TEX),6,27,221)
    else if (md_ident == 'equator1d') THEN
        CALL SETTEXTSIZEFAC(1.5D0)
        TEX =  ' ' ; ! u=C*sqrt(hi): h=10, i=10-4,  u=C*0.03162277

        !       1        1         2         3         4         5         6         7         8         9
        tex = 'Forced   amplitude =           (m), ndx = 360 , ndt = 360 , cfl =            '
        WRITE (TEX(22:30),'(F9.6)')  ampliforced
        WRITE (TEX(43:45),'(I3)')      ndxforced
        WRITE (TEX(55:58),'(I4)')      ndtforced
        WRITE (TEX(67:75),'(F9.6)' )   cflforced
        CALL ICTEXT(TRIM(TEX),6,24,ncolana)

        tex = 'Free     amplitude =           (m), ndx = 360 , ndt = 360 , cfl =            '
        WRITE (TEX(22:30),'(F9.6)')  amplifreeL
        WRITE (TEX(43:45),'(I3)')      ndxfreeL
        WRITE (TEX(55:58),'(I4)')      ndtfreeL
        WRITE (TEX(67:75),'(F9.6)')    cflfreeL
        CALL ICTEXT(TRIM(TEX),6,27,ncolana)

        tex = 'Total    amplitude =           (m)'
        WRITE (TEX(22:30),'(F9.6)')  amplitotal
        CALL ICTEXT(TRIM(TEX),6,30,ncolana)

        !       1        1         2         3         4         5         6         7         8         9
        tex = 'Computed amplitude =           (m), comp/analytic=  '
        amplicomp = 0.5d0 * (maxval(s1) - minval(s1) )
        WRITE (TEX(22:30),'(F9.6)')  amplicomp
        WRITE (TEX(52:60),'(F9.3)')  amplicomp / amplitotal
        CALL ICTEXT(TRIM(TEX),6,33,221)

        tex= 'Teta    =            , Manlin =            , Umod =            '
        WRITE (TEX(10:18),'(F9.5)')  teta0
        WRITE (TEX(32:40),'(F9.5)')  frcuni
        WRITE (TEX(52:60),'(F9.5)')  umodlin
        CALL ICTEXT(TRIM(TEX),6,36,221)

        tex= 'Nforced =            , Nfree  =            , Dept =            '
        WRITE (TEX(10:18),'(F9.2)')  Tforce
        WRITE (TEX(32:40),'(F9.2)')  TfreeL
        WRITE (TEX(52:60),'(F9.2)')  abs(zkuni)

        CALL ICTEXT(TRIM(TEX),6,39,221)

        call equatorial(time1)

    else if ( md_ident(1:6) == 'wetbed'    .or. md_ident(1:6) == 'drybed'   .or.  &
              md_ident(1:9) == 'thacker1d' .or. md_ident(1:8) == 'belanger' .or.  &
              md_ident(1:12) == 'coriolistilt' .or. md_ident(1:14) == 'corioliskelvin') THEN
       
        jaanalytic = 1

        TEX =  'Ave Difference (m) =             '
        WRITE (TEX(22:31),'(F10.4)')  avedif
        CALL ICTEXT(TRIM(TEX),4,31,ncolana)

        TEX =  'Rms Difference (m) =             '
        WRITE (TEX(22:31),'(F10.4)')  rmsdif
        CALL ICTEXT(TRIM(TEX),4,34,ncolana)

        TEX =  'Max Difference (m) =             '
        WRITE (TEX(22:31),'(F10.4)')  dmxdif
        CALL ICTEXT(TRIM(TEX),4,37,ncolana)

        IF (NUMCUM > 0) THEN
           TEX =  'Cum Difference (m) =             '
           WRITE (TEX(22:31),'(F10.4)')  cumavedif/numcum
           CALL ICTEXT(TRIM(TEX),4,40,ncolana)
        ENDIF

     else if ( md_ident == 'transport1d' .or. index(md_ident,'horvic') > 0) THEN

        jaanalytic = 1
        
        if (md_ident == 'transport1d' ) then 
           TEX =  'Ave Difference (ppt) =             '
        else 
           TEX =  'Ave Difference (m/s) =             '
        endif
        WRITE (TEX(24:33),'(F10.7)')  avedif
       ! CALL ICTEXT(TRIM(TEX),4,25,ncolana)

    else if (md_IDENT(1:5) == 'weir1') then  ! aligned channel  

        call weirtheo(1)

    else if (md_IDENT == 'weir2') then  ! 45 degree channel

        call weirtheo(2)

    else if (md_IDENT == 'gate1') then

        call gatetheo()
        
    else if (md_IDENT == 'riverfloodwave') then

        call riverfloodwave()    

    end if

    CALL SETTEXTSIZEFAC(1D0)
end subroutine textflowspecific 

subroutine update_turkin_modelspecific(Lf)
use m_flowgeom
use m_flow
use m_flowtimes
use unstruc_model, only: md_specific
implicit none
   integer, intent(in) :: Lf !< 2D flow link number

   double precision, save :: zw1(kmxx), uu(kmxx)
   double precision :: dummy
   integer :: L, Lb, Lt, kxL, mout, j
  
   Lb  = Lbot(Lf)                                   ! bed layer index
   Lt  = Ltop(Lf)                                   ! surface layer index = surface interface index
   kxL = Lt-Lb+1                                    ! nr of layers

   if (trim(md_specific) == 'splitter') then        ! Model: Splitter plate
      if (u1(Lt) >= 0d0 .and. Lf > lnxi) then       ! Boundary: velocity inflow side

         ! Prepare z/interface coords at link position.
         if (dnt <= 1d0 .and. Lf >= lnxi) then
            zw1(1) = 0d0
            do L = Lb,Lt
               zw1(L-Lb+2) = min(hu(L), hu(Lt))
            enddo
         endif
         ! call newfil(mout,'uprof.txt')
         do L = Lb-1,Lt
            call ENTRYFLOW( zw1, L-Lb+2, uu(L-Lb+2), dummy, turkin1(L), tureps1(L), dummy, dummy)
         end do
         ! write(mout,'(100f6.3)') ( zw1(j)/zw1(kxl+1), j = 1,kxL+1 )
         ! WRITE(MOUT,*) 'U'
         ! write(mout,'(100f6.3)') (  uu(j), j = 1,kxL+1 )
         ! call doclose  (mout)
         
      end if ! boundary links only
   ! else other models...
   end if ! model selection

end subroutine update_turkin_modelspecific

! AvD: TODO: cleanup below
subroutine equatorial(t)

use m_flowgeom
use m_flow
use sorting_algorithms, only:indexx
use geometry_module, only: dbdistance
use m_missing, only: dmiss
use m_sferic, only: jsferic, jasfer3D

implicit none

double precision                    :: t, deltax
double precision, allocatable, save :: uexa(:),zexa(:), xexa(:)
integer         , allocatable, save :: iexa(:)
integer                             :: L, k1, k2, n, i, lunfil

if (t == 0d0) then
   if (allocated (uexa) ) deallocate (uexa, zexa, xexa,iexa)
   allocate( uexa(ndx), zexa(ndx), xexa(ndx), iexa(ndx) )
   call indexx(ndx,xz,iexa)
   open (newunit=lunfil, file = 'eqa.txt')
endif

k1 = ln(1,1) ; k2 = ln(2,1)
deltax = dbdistance(xz(k1),yz(k1),xz(k2),yz(k2),jsferic, jasfer3D, dmiss)
call equatorialexact(t,xz,uexa,zexa,ndx,deltax)

if (t == 0d0) then
   s1(1:ndx) = zexa(1:ndx) ; s0=s1
   do L = 1,lnx
      k1 = ln(1,L) ; k2 = ln(2,L)
      u1(L) = 0.5d0*csu(L)*( uexa(k1) + uexa(k2) ) ; u0 = u1
   enddo
endif

if (t == 18000d0) then
   do n = 1,ndx
      i = iexa(n)
      if (n < 40) then
         write(lunfil,*) xz(i), zexa(i), s1(i)
      endif
   enddo
   close (lunfil)
endif

do L = 1,lnx
   k1 = ln(1,L) ; k2 = ln(2,L)
   if (abs(xz(k1) - xz(k2)) < 180d0) then
      call movabs(xz(k1),zexa(k1))
      call  lnabs(xz(k2),zexa(k2))
   endif
enddo


end subroutine equatorial

 subroutine equatorialexact(t,xz,uexa,zexa,nx,deltax)
 use m_sferic
 use m_physcoef
 use m_equatorial
 use m_flowtimes
 use m_netw, only  : zkuni

 implicit none
 integer          :: nx, n
 double precision :: t,xz(nx),uexa(nx),zexa(nx)
 Complex (kind=8) :: ep, em, cp, cm, eiomt, eiomx, sqrtc, tt, htt
 double precision :: tt0, F, cforc, cfree, per, lam, omfree
 double precision :: deltax, czf, reltim, relday, rnrel


 Ue0 = 0d0
 g  = ag
 h  = abs(zkuni)

 if (frcuni == 0d0) then
    fr  = 0
 else
    czf = h**0.1666666/frcuni
    fr  = ag*umodlin/(czf*czf*h)
 endif

 L      = twopi*Ra         ! earth's circumference in meters
 period = 24d0*60d0*60d0   ! number of seconds in a day

 lam    = L/nmode
 per    = period/nfreq ; Tforce = time1/per
 k      = twopi/lam
 om     = twopi/per


 ! deltax = dbdistance(0d0,0d0,1d0,0d0)
 cforc  = om/k         ; cflforced = cforc*dts/deltax
 ndxforced = lam/deltax
 ndtforced = per/dts; ndtforced = min(999, ndtforced) ! I4 print safety

 ufac   = om/(h*k)

 sqrtc  = sqrt(-fr** 2 + (0d0, 2d0) * fr * Ue0 * k + (Ue0 ** 2) * k ** 2 + 4 * h *( k ** 2) * g)

 ep     = (0.d0,0.5d0) * ( (0d0, 1d0)* fr + Ue0 * k + sqrtc)
 cp     =  app*exp( ep * t)

 em     = (0.d0,0.5d0) * ( (0d0, 1d0)* fr + Ue0 * k - sqrtc)
 cm     =  amm*exp( em * t)

 cfree  = sqrt(g*h)
 omfree = -imag(em); TfreeL = twopi/omfree ; TFreel = time1/TfreeL
 cfree  = omfree/k

 cflfreeL = cfree*dts/deltax
 ndxfreeL = ndxforced
 ndtfreeL = ndxfreeL/cflfreeL ; ndtfreeL = min(999, ndtfreeL) 


 reltim = -1d0/dreal(ep)    ! relaxation period
 if (time1  > reltim) then
     relday = reltim/(24*3600)
     rnrel  = reltim/(twopi/omfree)
 endif

 tt     = (h   * k ** 2 * g + (0d0, 1d0) * fr * om + Ue0 * k * om - om ** 2)
 tt0    = (h   * k ** 2 * g                        + Ue0 * k * om - om ** 2)

 htt    = ztyp

 ! F      = htt*tt/(h*k)

 F      = htt*tt0/(h*k) ; Zp = -F/k
 htt    =  h * k * F  / tt



 eiomt  = exp((0d0, 1d0 ) * om * t)
 do n = 1,nx
    x       = xz(n)*L/360d0       ! xx in degrees 0-360, => x in meters
    eiomx   = exp((0, -1) * k * x)
    uexa(n) = (0d0, -1d0) * ( em*cm + ep*cp - htt * om * eiomt) *  eiomx / (h*k)
    zexa(n) = (cm +  cp + (0d0, 1d0) * htt * eiomt ) * eiomx
 enddo

 ampliforced = abs(htt)
 amplifreel  = abs(cm)
 amplitotal  = abs((cm +  cp + (0d0, 1d0) * htt * eiomt ) )


! uexa = (0, -1) * ( (0., 0.5) * ((0, 1) * lambda + Ue0 * k - sqrtc ) *  cg1 + (0.0, 0.5) * ((0, 1) * lambda + Ue0 * k + sqrtc) * cg - 1D0 / (h * k ** 2 * g + (0, 1) * lambda * omega + Ue0 * k * omega - omega ** 2) * h * k * F * omega * exp((0, 1) * omega * t)) / h / k * exp((0, -1) * k * x)

! zexa = (cg1 +  cg + (0, 1) / (h *  k ** 2 * g + (0, 1) * lambda * omega + Ue0 * k * omega - omega **  2) * h * k * F * exp((0, 1) * omega * t)) * exp((0, -1) * k * x)



 end subroutine equatorialexact

subroutine riverfloodwave()
use m_flow
use m_flowgeom
use unstruc_colors
use m_flowtimes
use m_monitoring_crosssections
implicit none
character(len=132):: tex

double precision :: sqlabda, sqgi, tim, et, et0, uu, uu0,  ust, qst, qq 

sqlabda = sqrt(cfuhi(1))
sqgi    = sqrt(ag*abs(bedslope)) 
tim     = time1 - dts  
et0     = exp(-tim*2d0*sqgi*sqlabda) 
et      = exp(-time1*2d0*sqgi*sqlabda) 

ust     = sqgi / max(sqlabda,1d-10)

uu0     = ust*(1d0-et0) / (1d0+et0) 
uu      = ust*(1d0-et ) / (1d0+et ) 

uu      = teta0*uu + (1d0-teta0)*uu0

qq      = au(1)*uu 
qst     = au(1)*ust


TEX =  'Q-analytic (m3/s)      : '
WRITE (TEX(26:36),'(F11.4)') qq
CALL ICTEXT(TRIM(TEX),5,24,NCOLANA)

if (abs( crs(1)%sumvalcur(IPNT_Q1C) ) > 0) then 
TEX =  'Numerical/Analytic ( ) : '
WRITE (TEX(26:36),'(F11.4)') qq / abs(crs(1)%sumvalcur(IPNT_Q1C) ) 
CALL ICTEXT(TRIM(TEX),5,27,NCOLANA)
endif

TEX =  'Steady state ratio     : '
WRITE (TEX(26:36),'(F11.4)') qq/qst
CALL ICTEXT(TRIM(TEX),5,30,NCOLANA)

TEX =  'Time (hours)           : '
WRITE (TEX(26:36),'(F11.4)') time1/3600d0
CALL ICTEXT(TRIM(TEX),5,33,NCOLANA)

end subroutine riverfloodwave

subroutine weirtheo(j12)
use m_flow
use m_flowgeom
use unstruc_colors
use m_observations
use m_monitoring_crosssections
use m_flowtimes
use unstruc_model, only: getoutputdir
implicit none
integer, intent(in) :: j12
integer             :: k, L, LL, num  , kk, k1, k2, Lweir, ncgentst
double precision    :: slinks,srechts, eup, edo, dE, dH, foot, zg, z1, z2, z3, qg, a, cc, f1, f2, qglab, z2lab, qsimple, tim
double precision    :: zupstream,zdownstream,crestheight,zcrestperfect,zminsub,zcrest, submer,qfree, g=9.81d0 , qg12, qgen, zg12   
double precision    :: qweirana,qweirc,uupstream,ucrest,udownstream,bedlev,crestlev, qsub, qsup, qcond, qthd, gateheight, qcrit
double precision    :: qrajaratnam
character(len=132)  :: tex
character(len= 32)  :: regime
character(len= 15)  :: datetime

integer, save       :: mout = 0, nt = 0, minp = 0, mou2 = 0

if (mout == 0) then 
   tex = 'qweirs6.out'
   write(tex(7:7) , '(i1.1)' ) ifixedweirscheme  
   call newfil(mout, trim(getoutputdir())//tex)
   write(mout,'(A)') ' submergence  analytic  subgrid'   
endif

slinks      = s1(kobs(1))           
srechts     = s1(kobs(3))
bedlev      = bl(kobs(1)) 
crestlev    = 1d0

Lweir = 0
do L = 1,lnx
   if (iadv(L) == 21 .or. iadv(L) >= 23 .and. iadv(L) <= 25) then 
      crestlev  = min( bob(1,L), bob(2,L) )    
      Lweir     = L ; exit  
   endif   
enddo    

if (Lweir == 0) then 
   return 
else if (hu(Lweir) == 0) then 
   return 
endif

if (ncgen > 0) then 
   crestlev   = zcgen(1)
   gateheight = zcgen(2) - crestlev
endif
   
crestheight = crestlev - bedlev  
zupstream   = slinks   - crestlev
zdownstream = srechts  - crestlev
if (abs(zupstream-zdownstream) < 0.0001) then
   return
endif

regime      = 'subcritial'

qweirana = 0d0 ;dE = 0d0 ; uupstream = 0d0 ; udownstream = 0d0

ncgentst = -1
if (ncgentst > 0) then 

   foot       = 0.3048d0

   if (minp == 0) then 
      call oldfil(minp, 'rajaratnam1967.data')
      call newfil(mou2, 'rajaratnam1967.txt')
   
      do k = 1, 37
         read(minp, *) gateheight, z1, z2lab, z3, qglab
         gateheight = foot*gateheight 
         z1         = foot*z1
         z2lab      = foot*z2lab
         z3         = foot*z3
         
         z3         = z1*0.5d0 

         crestheight = max(0.3d0*z1, z1 - gateheight) 
         z3          = crestheight + 0.9d0*(z1 - crestheight) 
         z3 = 0.5d0*z3
         
         qglab       = foot*foot*qglab
         call findqorifice  (gateheight,crestheight,z1,z3,qg,z2,zg,regime,num,qcrit)  
         call findqorifice12(gateheight,crestheight,z1,z2lab,qg12,zg12,regime,num,qcrit) 
        ! if (num .ne. 50) then 
            qsimple     = gateheight*sqrt( 2d0*g*(z1-z3) )
            tim = 1440d0*k - 23*60 ; call maketime(datetime, tim*60d0)
            write(mou2,'(A,20F8.3)') datetime, qglab*10d0, qg*10d0, qg12*10d0, gateheight, crestheight, z1, z3, z2, qglab, qg, qg12, qg/qglab, qg12/qglab, zg/gateheight, zg12/gateheight   
            tim = 1440d0*k         ; call maketime(datetime, tim*60d0)
            write(mou2,'(A,20F8.3)') datetime, qglab*10d0, qg*10d0, qg12*10d0, gateheight, crestheight, z1, z3, z2, qglab, qg, qg12, qg/qglab, qg12/qglab, zg/gateheight, zg12/gateheight  
        ! endif   
      enddo   
         
   endif 
   
   z1 = zupstream ; z3 = zdownstream ; qg = 1d0
   call findqorifice(gateheight,crestheight,z1,z3,qg,z2,zg,regime,num,qcrit)  
   qweirana = qg; ucrest = qg/zg; zcrest = z2; qfree = qcrit 
   
   if (mod (time1, 60000d0 ) == 0) then 
   
      qgen = q1(19)/wu(19) 
      write(mou2, *) time1, qgen 
   
   endif
   
else 

   gateheight = 9d9

   call weirtheory(zupstream,zdownstream,crestheight,zcrestperfect,zminsub,zcrest,  &
                qweirana,uupstream,ucrest,udownstream, regime, qfree, gateheight)
   
   qrajaratnam = zdownstream * sqrt(2d0*ag*(max(0d0, zupstream - zdownstream ) ) ) 
   
endif

if (qweirana == 0d0) return

eup = zupstream   + 0.5d0*uupstream*uupstream/ag
edo = zdownstream + 0.5d0*udownstream*udownstream/ag 
dE  = eup - edo   ; dH = zupstream - zdownstream  
dE  = max(0d0, dE) ; dH = max(0d0, dH)  

TEX  =  'Uup :            Ucr :               Udown :                '
WRITE (TEX( 7:16),'(f10.3)')  uupstream  
WRITE (TEX(24:33),'(f10.3)')  ucrest 
WRITE (TEX(46:55),'(f10.3)')  udownstream  
CALL ICTEXT(TRIM(TEX),5,14,Ncolana)

TEX  =  'Zup :            Zcr :               Zdown :                '
WRITE (TEX( 7:16),'(f10.3)')  zupstream      
WRITE (TEX(24:33),'(f10.3)')  zcrest 
WRITE (TEX(46:55),'(f10.3)')  zdownstream  
CALL ICTEXT(TRIM(TEX),5,16,Ncolana)

TEX  =  'Eup :            Ecr :               Edown :                '
WRITE (TEX( 7:16),'(f10.3)')  eup   
WRITE (TEX(24:33),'(f10.3)')  zcrest + 0.5d0*ucrest*ucrest/ag     
WRITE (TEX(46:55),'(f10.3)')  edo 
CALL ICTEXT(TRIM(TEX),5,18,Ncolana)

TEX =  'Q-analytic     : '
WRITE (TEX(18:27),'(F10.3)') qweirana
WRITE (TEX(31:),'(A,F5.3,A,F5.3,A,F5.3)'    )  & 
        regime(1:3)//' ; Q/Qf = ', qweirana/qfree, ' S = ', zdownstream/zupstream,  ' dH = ', dH 
CALL ICTEXT(TRIM(TEX),5,20,Ncolana)


if (j12 == 1) then    
   TEX =  'Q supergrid    :            (m2/s); Q/Qana =                              '
   qsup = 0.1D0*crs(4)%sumvalcur(IPNT_Q1C) ! specific
   WRITE (TEX(18:27),'(f10.3)')  qsup
   WRITE (TEX(47:56),'(f10.3)')  qsup/qweirana
   CALL ICTEXT(TRIM(TEX),5,22,221)

   TEX =  'Q nothing      :            (m2/s); Q/Qana =                              '
   qsub = 0.1D0*crs(3)%sumvalcur(IPNT_Q1C) ! specific
   WRITE (TEX(18:27),'(f10.3)')  qsub
   WRITE (TEX(47:56),'(f10.3)')  qsub/qweirana
   CALL ICTEXT(TRIM(TEX),5,24,221)

   TEX   =  'Q General      :            (m2/s); Q/Qana =                              '
   qcond = 0.1D0*crs(2)%sumvalcur(IPNT_Q1C) ! specific
   WRITE (TEX(18:27),'(f10.3)')  qcond
   WRITE (TEX(47:56),'(f10.3)')  qcond/qweirana
   CALL ICTEXT(TRIM(TEX),5,26,221)

   TEX  =  'Q fixedweir    :            (m2/s); Q/Qana =                              '
   qthd = 0.1D0*crs(1)%sumvalcur(IPNT_Q1C) ! specific
   WRITE (TEX(18:27),'(f10.3)')  qthd 
   WRITE (TEX(47:56),'(f10.3)')  qthd/qweirana
   CALL ICTEXT(TRIM(TEX),5,28,221)
            
   nt = nt + 1
   if (mod(nt, 100) == 0) then 
     if (zupstream > 0) then 
       submer = zdownstream/zupstream
       write(mout,'(6F9.6)') submer, Qweirana/qfree,  qthd/qfree   ! qsup/qfree, qsub/qfree,
     else
       submer = 0d0
     endif
   endif
   
else    

   TEX =   'Q fixedweir  :            (m2/s); Q/Qana =                              '
   qthd     = crs(1)%sumvalcur(IPNT_Q1C) / (80d0*sqrt(2d0) ) ! specific
   WRITE (TEX(18:27),'(f10.3)')  qthd
   WRITE (TEX(47:56),'(f10.3)')  qthd/qweirana
   CALL ICTEXT(TRIM(TEX),5,22,221)
   
   nt = nt + 1
   if (mod(nt, 100) == 0) then 
     if (zupstream > 0) then 
       submer = zdownstream/zupstream
       if (submer > 0.4 ) then 
         write(mout,'(3F9.6)') submer, Qweirana/qfree, qthd/qfree  
       endif  
     endif  
   endif
   
endif

end subroutine weirtheo



subroutine gatetheo()
use m_flowgeom
use m_flow

use unstruc_colors

implicit none
double precision  :: agate,hup,hdown,qgate,hgate, z2, zg, qcrit
integer           :: num

character(len=132):: tex, regime


hup = s0(64); hdown= s0(62) ; agate = zgate(1)
if (hup > hdown + 0.0001) then

   call findqorifice(agate,0d0,hup,hdown,qgate,z2,zg,regime,num,qcrit)

   TEX =  'Q-Analytic bot :            (m2/s); '//regime
   WRITE (TEX(18:27),'(f10.3)')  qgate
   CALL ICTEXT(TRIM(TEX),5,34,ncolana)
   TEX =  'Q bot channel  :            (m2/s); Q/Qana =                              '
   WRITE (TEX(18:27),'(f10.3)')  0.1*q1(14)
   WRITE (TEX(47:56),'(f10.3)')  0.1*q1(14)/max(1d-4,qgate)
   CALL ICTEXT(TRIM(TEX),5,36,221)
   
endif


end subroutine gatetheo



subroutine poiseuille(init)
 use m_netw
 use m_flowgeom
 use m_flow
 use m_flowtimes
 use unstruc_model
 use m_monitoring_crosssections
 use m_alloc
 use unstruc_colors, only: ncolana 
 use m_statistics, only: avedif 
 use sorting_algorithms, only: indexx
 use geometry_module, only: dbdistance
 use m_missing, only: dmiss
 use m_sferic, only: jsferic, jasfer3D

 implicit none

 integer, intent(in)                           :: init    !< whole flow-field initialization (1) or velocity boundary condition only (0)

 character(len=64)                             :: FNAM    ! crosssection output filename
 character(len=64)                             :: varname1, varname2, varname3, varname4  ! variable names

 double precision, allocatable, dimension(:)   :: dcrs    ! cross-section polygon coordinate
 double precision, allocatable, dimension(:,:) :: var     ! for output
 double precision, allocatable, dimension(:)   :: var1    ! for output

 integer,          allocatable, dimension(:)   :: perm    ! for sorting

 double precision                              :: hev, c0, b, dy, hdy, dyy, aa, yw, ust, c2a, c2, xx0, yy, yf, uu, xx, botinc, fac
 double precision                              :: x1, y1, xL, yL, xR, yR, alpha
 double precision                              :: uxL, uyL, uxR, uyR, ux1, uy1, s01, yy0

 integer                                       :: i, j, k, ki, L, num
 integer                                       :: icrs, np, nl, ip, ip1, kL, kR

 integer,          save                        :: icount
 double precision, save                        :: time2write = -1d99
 logical,          save                        :: Lwriteheader = .true., Lheaderwritten = .false.
 logical                                       :: Lwritetime

 integer                                       :: fid

 character (len=40)                            :: tex
 double precision                              :: sumba
 
 integer                                       :: ndraw

 common /drawthis/ ndraw(50)
 
 if ( abs(bedslope).lt.1d-8 ) bedslope = -0d-5  ! SPvdP: now old mdu-files still work

 hev = vicouv                   ! horizontal eddy viscosity

 c0  = -0.5d0*ag*bedslope       ! g*i/2

 b   = 100d0                    ! channel width

 dy  = 10                       ! cellsize near wall

 hdy = 0.5d0*dy                 

 yw  = b/2 - hdy

 if (irov == 1) then       ! uniform mesh assumed, with cell size near wall dy (see above)
    aa  = vonkar/log(c9of1 + hdy/wall_z0)
    !aa  = vonkar/log(1d0   + hdy/wall_z0)

    ust = sqrt(abs(c0*b))

    c2a = ust*hev/aa

    c2  = c2a + c0*yw*yw

 else if (irov == 2) then 
    c2 = c0*(b/2)**2
 endif

 c0  = c0/max(hev,1d-8) ; c2=c2/max(hev,1d-8)

 xx0 = minval(xk) 
 yy0 = maxval(yk) + 3*dy  
 
 call movabs(xx0,yy0)
 call lnabs( maxval(xk), yy0)
 
 call movabs(xx0,yy0)
 call lnabs( xx0,yy0 + maxval(yk) - minval(yk) )


 dyy = b/100 
 do k   = 1,101
  
    yy  = (k-1)*dyy
    yf  = abs( b/2 - yy)
    uu  = -c0*yf*yf + c2
    xx  = xx0 + 50*uu
    yy  = yy0 + yy
    if (k == 1) then
       call movabs(xx,yy)
    else
       call lnabs(xx,yy)
    endif
 enddo


 if ( init==1 ) then
!   initialization
    do L=1,lnx
       yy = yu(L)
       yf = abs( b/2 - yy)
       uu = -c0*yf*yf + c2
       u1(L) = uu*csu(L)
    end do
    Lwriteheader   = .true.
    Lheaderwritten = .false.
    time2write = time0
    
    ndraw(18) = 4+1   ! SPvdP: plot profiles, compute analytic solution
else 
!   velocity boundary condition only
    if ( vol0tot.gt.0d0 ) then
       fac = 4d5/vol0tot
    else
       fac = 1d0
    end if

    do k = 1,nbndu
       yy = ybndu(k)
       yf = abs( b/2 - yy)
       uu = -c0*yf*yf + c2
       L  = kbndu(3,k)
       ki = kbndu(2,k)
!       zbndu(k) = uu*10d0 / (s1(ki)+10d0)
!       zbndu(k) = fac*uu
       zbndu(k) = uu
    enddo

    do k=1,nbndn
       yy = ybndn(k)
       yf = abs( b/2 - yy)
       uu = -c0*yf*yf + c2
       zbndn(k) = uu
    end do

 end if

 avedif = 0d0; sumba  = 0d0
 do k = 1,ndxi
    yy = yy0 + yz(k)
    if (xz(k) > 390d0) then
       xx = xx0 + 50*sqrt( ucx(k)*ucx(k) + ucy(k)*ucy(k) ) 
       call rcirc(xx,yy)
    endif
    yf  = abs( b/2 - yy)
    uu  = -c0*yf*yf + c2
    
    plotlin(k) = ucx(k) - uu 
    avedif = avedif + dabs( ucx(k) - uu)*ba(k)
    sumba  = sumba  + ba(k)  

 enddo
 avedif = avedif/sumba

 CALL SETTEXTSIZEFAC(2D0)
 TEX =  'Ave Difference (m/s) =             '
 WRITE (TEX(24:33),'(F10.7)')  avedif
 !CALL ICTEXT(TRIM(TEX),4,25,ncolana)
 CALL SETTEXTSIZEFAC(1D0)

!  write cross-section data to file
 if ( time0 .ge. time2write ) then

    num = len_trim(md_ident)
    FNAM = ''
    FNAM(1:num) = md_ident(1:num)
    FNAM(num+1:num+2) = '.m'

!   open file
    if ( Lwriteheader ) then
       open(newunit=fid, file=trim(FNAM))
    else
       open(newunit=fid, file=trim(FNAM), access="append")
    end if

!   allocate
    allocate(dcrs(1), perm(1), var(2,1), var1(1))

    Lwritetime = .true.   ! we want to write the time once

!   loop over the cross sections
    do icrs=1,ncrs
       np = crs(icrs)%path%np
       nl = crs(icrs)%path%lnx

   !   reallocate if necessary
       num = ubound(dcrs,1)
       if ( nl.gt.num ) then
         num = int(1.2*nl)+1
         call realloc(dcrs,num)
         call realloc(perm,num)
         call realloc(var,(/2,num/))
         call realloc(var1,num)
       end if

   !   sort crossings in increasing polygon coordinate
       if ( nl.gt.0 ) then
          do i=1,nl
            dcrs(i) = dble(crs(icrs)%path%indexp(i)) + (1d0-crs(icrs)%path%wfp(i))
          end do
          call indexx(nl,dcrs,perm)
       else
          cycle
       end if

       if ( Lwriteheader ) then
   !      write header: coordinates and exact solution
          do j=1,nl
            i = perm(j)
            ip1 = crs(icrs)%path%indexp(i)

            xL  = crs(icrs)%path%xp(ip1)
            xR  = crs(icrs)%path%xp(ip1+1)
            yL  = crs(icrs)%path%yp(ip1)
            yR  = crs(icrs)%path%yp(ip1+1)

            alpha = 1d0 - crs(icrs)%path%wfp(i)

            x1 = xL + alpha * (xR-xL)
            y1 = yL + alpha * (yR-yL)

            var(1,j) = x1
            var(2,j) = y1

!           exact solution
            yf = abs( b/2 - y1)
            uu = -c0*yf*yf + c2

            var1(j) = uu
          end do
          
   !      construct the variable names
          varname1 = ''
          varname2 = ''
          varname3 = ''
          varname4 = ''
          if ( icrs.lt.10 ) then
             write(varname1, "('x{',   I1, '}')") icrs
             write(varname2, "('uex{', I1, '}')") icrs
          else if ( icrs.lt.100 ) then
             write(varname1, "('x{',   I2, '}')") icrs
             write(varname2, "('uex{', I2, '}')") icrs
          else
             write(varname1, "('x{',   I3, '}')") icrs
             write(varname2, "('uex{', I3, '}')") icrs
          end if

          call matlab_write_double(fid, varname1, var, 2, nl)
          call matlab_write_double(fid, varname2, var1, 1, nl)

          Lheaderwritten = .true.
       else if ( Lheaderwritten ) then
   !      write time and profiles of velocity components and water height

   !      construct the variable names
          varname1 = ''
          varname2 = ''
          varname3 = ''
          varname4 = ''
          if ( Lwritetime ) then
            write(varname1, "('t(', I10, ')')")  icount
            write(varname4, "('Dt(', I10, ')')") icount
          end if
          if ( icrs.lt.10 ) then
             write(varname2, "('u{', I1, '}', '(:,:,', I10, ')')") icrs, icount
             write(varname3, "('s{', I1, '}', '(:,',   I10, ')')") icrs, icount
          else if ( icrs.lt.100 ) then
             write(varname2, "('u{', I2, '}', '(:,:,', I10, ')')") icrs, icount
             write(varname3, "('s{', I2, '}', '(:,',   I10, ')')") icrs, icount
          else
             write(varname2, "('u{', I3, '}', '(:,:,', I10, ')')") icrs, icount
             write(varname3, "('s{', I3, '}', '(:,',   I10, ')')") icrs, icount
          end if

          do j=1,nl
            i = perm(j)
            ip1 = crs(icrs)%path%indexp(i)

            xL  = crs(icrs)%path%xp(ip1)
            xR  = crs(icrs)%path%xp(ip1+1)
            yL  = crs(icrs)%path%yp(ip1)
            yR  = crs(icrs)%path%yp(ip1+1)

            alpha = 1d0 - crs(icrs)%path%wfp(i)

            x1 = xL + alpha * (xR-xL)
            y1 = yL + alpha * (yR-yL)

   !        flow link
            L = abs(crs(icrs)%path%ln(i))

   !        cell centers
            kL = ln(1,L)
            kR = ln(2,L)

   !        cell centers
            xL = xz(kL)
            yL = yz(kL)
            xR = xz(kR)
            yR = yz(kR)

   !        weight factor
            alpha = dbdistance(xL,yL,x1,y1, jsferic, jasfer3D, dmiss)/dbdistance(xL,yL,xR,yR,jsferic, jasfer3D, dmiss)

            ux1 = ucx(kL) + alpha*(ucx(kR)-ucx(kL))
            uy1 = ucy(kL) + alpha*(ucy(kR)-ucy(kL))

            s01 = s0(kL) + alpha*(s0(kR)-s0(kL))

            var(1,j) = ux1
            var(2,j) = uy1

            var1(j)   = s01
          end do
          if ( Lwritetime ) then
             call matlab_write_double(fid, varname1, (/time0/), 1, 1)
             call matlab_write_double(fid, varname4, (/dts  /), 1, 1)
          end if
          call matlab_write_double(fid, varname2, var, 2, nl)
          call matlab_write_double(fid, varname3, var1, 1, nl)

          Lwritetime = .false.   ! we want to write the time only once
       end if
    end do  ! do icrs=1,ncrs

!   close file
    close(fid)

!   determine next time to write
    if ( Lheaderwritten ) then
       if ( Lwriteheader ) then
          Lwriteheader = .false.
          icount = 1
          time2write     = 0d0
       else
          icount = icount+1
          time2write = time2write+ti_map
       end if
    end if
 end if  ! if time0.gt.time2write

! deallocate
 if ( allocated(dcrs) ) deallocate(dcrs)
 if ( allocated(perm) ) deallocate(perm)
 if ( allocated(var)  ) deallocate(var)
 if ( allocated(var1) ) deallocate(var1)

 end subroutine poiseuille
