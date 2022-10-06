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

! $Id: setprofs1d.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/setprofs1d.f90 $

 subroutine setprofs1D()

 use m_flowgeom
 use m_flow
 use UNSTRUC_MODEL
 use m_netw
 use m_profiles
 use m_missing
 use unstruc_messages
 use m_partitioninfo
 use sorting_algorithms, only: indexx
 use geometry_module, only: dbdistance
 use m_sferic, only: jsferic, jasfer3D
 use m_samples

 IMPLICIT NONE
 integer                       :: ierr, MINP, LS, L, K, LF, IBR, LL,LA,K1,K2,KA,KB,NRL,NSK, KK, ja, ium
 DOUBLE PRECISION              :: XL, ZL, ALFA
 logical                       :: jawel
 character (len=256)           :: fnam
 INTEGER, allocatable          :: LSAM(:)              ! sample K IS ON NET LINK LSAM
 INTEGER, allocatable          :: NSbr (:)             ! nr of profiles on branch

 INTEGER                       :: NSBRMX               ! MX NR OF PROFILES ON BRANCH
 INTEGER, allocatable          :: IDX(:)               ! INDEX ARR, SIZE = NSBRMX
 INTEGER, allocatable          :: KLH(:), KLHH(:)      ! INDEX  ARR, + SORTED BY IDX
 double precision, allocatable :: XLH(:), XLHH(:)      ! LENGTH ARR, + SORTED BY IDX
 double precision, allocatable :: ZLH(:), ZLHH(:)      ! VALUE  ARR, + SORTED BY IDX
 integer                       :: nyz

 type tKBSAM                                           !< TEMP
     integer, allocatable      :: KS (:)               !< successive SAMPLE nrs ON BRANCH
 end type tKBSAM
 TYPE (TKBSAM),    dimension(:), ALLOCATABLE :: KBSAM  ! ARRAY OF SAMPLES PER BRANCH


 double precision, dimension(:), allocatable :: XLLin
 double precision, dimension(:), allocatable :: XLsam          ! link and sample line distances

 double precision, dimension(:), allocatable :: distsam  ! distance from sample to link
 integer,          dimension(:), allocatable :: iconnsam ! globally connected branch number associated with sample

 double precision, dimension(:), allocatable :: zkk, wkk ! help interpolate zk in profiles if dmiss

 double precision                            :: XLS, YLS, XLB, DXB, dum, ZA, ZB, wa, wb, zul, wul
 double precision                            :: wn(100), zn(100)
 integer                                     :: np(100) , nn, n1, kn3now

 if ( jampi.ne.1 ) then
    if (lnx1D == 0) return
 end if

 fnam =  trim(md_proflocfile)
 inquire(file=trim(fnam) , exist=jawel)

 if (jawel) then

    call oldfil(minp, fnam )

    call readprofilesloc(minp)           ! read profloc

    call readprofilesdef(ja)             ! read profdef

    if (ja == 0) then
       call qnerror( ' Profs not ok. ', ' ',' ')
       return
    endif

    if (nproflocs > 0) then

       CALL SETBRANCH_LC(ium)

       if ( jampi.eq.1 ) then
         call global_netbranch_numbering()
       end if

       if ( allocated(xllin) ) then
           deallocate(xllin)
       endif
       ierr = 0
       allocate ( XLLIN(numL) , stat = ierr)
       call aerr('XLLIN(numL)', ierr, numL )

       allocate ( XLSAM(Nproflocs)   , stat = ierr)
       call aerr('XLSAM(Nproflocs)  ', ierr, Nproflocs  )

       allocate ( LSAM(Nproflocs)    , stat = ierr)
       call aerr('LSAM(Nproflocs)   ', ierr, Nproflocs  )

       if ( jampi.eq.1 ) then
         allocate(distsam(Nproflocs), stat=ierr)
         call aerr('distsam(Nproflocs)', ierr, Nproflocs)

         allocate(iconnsam(Nproflocs), stat=ierr)
         call aerr('iconnsam(Nproflocs)', ierr, Nproflocs)
       end if

       xlsam = 0d0
       distsam = 1d99
       iconnsam = 0
       if ( Lnx1D.gt.0 ) then
          do ibr = 1,mxnetbr                                    ! SET UP BRANCH DISTANCE COORDINATE
             if ( jampi.eq.0 ) then
                XLB = 0d0
             else
                XLB = netbr(ibr)%doff
             end if
             do LL = 1, netbr(ibr)%NX
                L  = netbr(ibr)%ln(LL); LA = iabs(L)
                if (L > 0) then
                   k1 = kn(1,La); k2 = kn(2,LA)
                else
                   k2 = kn(1,La); k1 = kn(2,LA)
                endif
                dxB = dbdistance( xk(k1), yk(k1), xk(k2), yk(k2), jsferic, jasfer3D, dmiss)
                XLB = XLB + dxB
                XLLIN(LA) = xLB
             enddo
          enddo

          DO K = 1,nproflocs                                    ! SET UP BRANCH DISTANCE COORDINATE OF SAMPLE POINTS
             if ( jampi.eq.0 ) then
                CALL CLOSETO1Dnetlink(Xpr(K),Ypr(K),LS,XLS,YLS,dum, 1)
             else
                CALL CLOSETO1Dnetlink(Xpr(K),Ypr(K),LS,XLS,YLS,distsam(k), 1)
                if (LS > 0) then
                   ibr = LC(LS)
                   iconnsam(k) = netbr(ibr)%iconn
                end if
             end if

             if (LS == 0) then
                cycle
             end if

             NRL = NRLB(LS)
             K1  = K1BR(NRL)
             IF (K1 == KN(1,LS) )THEN                           ! K1 = FIRST IN BRANCH, K2 = SECOND
                 K2 =  KN(2,LS)
             ELSE
                 K2 = K1
                 K1 = KN(2,LS)
             ENDIF

             XLSAM(K) = XLLIN(LS) - DBDISTANCE(XLS,YLS,XK(K2),YK(K2), jsferic, jasfer3D, dmiss)

             LSAM(K) = LS

             !DO L = 1,NUML
             !   IF (LC(L) == LC(LS)) THEN
             !      LF = LNE2LN(L)
             !      PROF1D(1,LF) = ZS(K)
             !   ENDIF
             !ENDDO

          ENDDO

       end if

! parallel: reduce XLSAM and the connected branch numbers
       if ( jampi.eq.1 ) then
          call reduce_xlsam(Nproflocs, xlsam, distsam, iconnsam)
!       else
!          write(6,*) (xlsam(k), k=1,Nproflocs)
       end if

       if ( Lnx1D.gt.0 ) then
          do ibr = 1,mxnetbr                                    ! SET UP BRANCH AGAIN, NOW WITH LINK POSITIONS
             if ( jampi.eq.0 ) then
                XLB = 0d0
             else
                XLB = netbr(ibr)%doff
             end if
             do LL = 1, netbr(ibr)%NX
                L  = netbr(ibr)%ln(LL); LA = iabs(L)
                if (L > 0) then
                   k1 = kn(1,La); k2 = kn(2,LA)
                else
                   k2 = kn(1,La); k1 = kn(2,LA)
                endif
                dxB = dbdistance( xk(k1), yk(k1), xk(k2), yk(k2), jsferic, jasfer3D, dmiss)
                XLB = XLB + dxB
                XLLIN(LA) = xLB - 0.5D0*DXB
             enddo
          enddo

          ALLOCATE ( NSBR (MXNETBR) , STAT=IERR) ;  NSBR = 0
          CALL AERR('NSBR (MXNETBR)', IERR, MXNETBR)
          ALLOCATE ( KBSAM(MXNETBR) , STAT=IERR)
          CALL AERR('KBSAM(MXNETBR)', IERR, MXNETBR)

          DO K   = 1,Nproflocs                                    ! COUNT NR OF SAMPLES PER BRANCH
             L   = LSAM(K)
             IBR = LC(L)
             if ( jampi.eq.0 ) then
                NSBR(IBR) = NSBR(IBR) + 1
             else
   !            parallel: add profile to all branches that are in the corresponding connected branch
                do ibr=1,mxnetbr
                   if ( iconnsam(k).eq.netbr(ibr)%iconn ) then
                      NSBR(IBR) = NSBR(IBR) + 1
                   end if
                end do
             end if
          ENDDO

          DO IBR = 1,MXNETBR                                      ! ALLOC BRANCH SAMPLES BACKREF.
             IF (NSBR(IBR)  > 0) THEN
                ALLOCATE(KBSAM(IBR)%KS(NSBR(IBR) ) )
             ENDIF
          ENDDO

          NSBR   = 0; NSBRMX = 0
          DO K   = 1,Nproflocs                                    ! REFER BACK TO SAMPLES ON BRANCH
             L   = LSAM(K)
             IBR = LC(L)
             if ( jampi.eq.0 ) then
                NSBR(IBR) = NSBR(IBR) + 1
                KBSAM(IBR)%KS(NSBR(IBR)) = K
                NSBRMX = MAX( NSBRMX,NSBR(IBR) )
             else
                do ibr=1,mxnetbr
                   if ( iconnsam(k).eq.netbr(ibr)%iconn ) then
                      NSBR(IBR) = NSBR(IBR) + 1
                      KBSAM(IBR)%KS(NSBR(IBR)) = K
                      NSBRMX = MAX( NSBRMX,NSBR(IBR) )
                   end if
                end do
             end if
          enddo

          ALLOCATE ( KLH (NSBRMX), XLH (NSBRMX), ZLH (NSBRMX), IDX(NSBRMX) )
          ALLOCATE ( KLHH(NSBRMX), XLHH(NSBRMX), ZLHH(NSBRMX)              )

          DO IBR = 1,MXNETBR                                      ! ORDER SAMPLES ON BRANCH AND INTERP LINKS INTO IT

             IF (NSBR(IBR) > 0) THEN                              ! ER ZITTEN PROFIELEN OP
                DO KK = 1,NSBR(IBR)
                   K  = KBSAM(IBR)%KS(KK)
                   XLH(KK)  = XLSAM(K)
                   !ZLH(KK)  = Zpr(K)
                   KLH(KK)  = K
                ENDDO
                CALL INDEXX(NSBR(IBR),XLH,IDX)
                DO KK = 1,NSBR(IBR)                               ! NU GESORTEERD NAAR AFSTAND
                   XLHH(KK) = XLH(IDX(KK))
                   !ZLHH(KK) = ZLH(IDX(KK))
                   KLHH(KK) = KLH(IDX(KK))
                ENDDO

                K1 = 0; K2 = 1
                DO LL = 1, netbr(ibr)%NX
                   ! NOTE: vulnerability: netbr(:)%ln(:) contains NETlinks (see SETBRANCH_LC()), but it is used below as FLOWlinks
                   !       Not a problem as long as *no* netlinks are discarded during geominit. (Then: numl1d == lnx1d.)
                   LA = IABS( NETBR(IBR)%LN(LL) )
                   XL = XLLIN(LA)
                   DO WHILE (XL > XLHH(K2) .AND. K2 < NSBR(IBR) )
                      K2 = K2 + 1; K1 = K1 + 1
                   ENDDO

                   IF ( XL > XLHH(K2) ) THEN
                      K1 = K2
                   ENDIF

                   IF (K1 == 0) THEN                             ! IN FIRST SEGMENT, VALUE IS THAT OF K1
                      ALFA = 0D0
                   ELSE IF (K1 == NSBR(IBR) ) THEN               ! IN LAST  SEGMENT, VALUE IS THAT OF K2
                      ALFA = 1D0
                   ELSE                                          ! IN BETWEEN, REGULAR INTERPOLATION
                      ALFA = ( XL - XLHH(K1) ) / ( XLHH(K2) - XLHH(K1) )
                   ENDIF
                   IF (K1 == 0) THEN
                       KA = KLHH(K2)
                   ELSE
                       KA = KLHH(K1)
                   ENDIF
                   IF (K1 == NSBR(IBR)) THEN
                       KB = KLHH(K1)
                   ELSE
                       KB = KLHH(K2)
                   ENDIF
                   KA = NPR(KA) ; KB = NPR(KB)
                   IF (profiles1D(ka)%ityp <= 3 .and. profiles1D(ka)%ityp  == profiles1D(kb)%ityp ) THEN   ! identical simple profs are interpolated immediately
                       PROF1D(1,LA) = (1D0-alfa)*profiles1D(ka)%width  + alfa*profiles1D(kb)%width
                       PROF1D(2,LA) = (1D0-alfa)*profiles1D(ka)%height + alfa*profiles1D(kb)%height
                       PROF1D(3,LA) = PROFILES1D(KA)%ITYP
                   ELSE                                                                                    ! POINTEREN VOOR YZPROF OR MIXED PROFILE TYPES
                       PROF1D(1,LA) = -KA
                       PROF1D(2,LA) = -KB
                       PROF1D(3,LA) = ALFA
                   ENDIF

                   !call mess('profile interpolation ready',nproflocs)                                     ! EN NU IS DE INTERPOLATIE KLAAR

                ENDDO

             ENDIF

          ENDDO

          DEALLOCATE ( IBN  , LIB  , K1BR , NRLB)
          DEALLOCATE ( KLH  , XLH  , ZLH  , IDX )
          DEALLOCATE ( KLHH , XLHH , ZLHH       )

          IF (jainterpolatezk1D > 0) THEN

              allocate ( zkk(numk), wkk(numk))
              do kn3now = 6,1,-5
                 wkk = 0d0 ; zkk = 0d0
                 do L = 1, lnx1D
                    if (abs(kcu(L)) == 1 .and. kn(3,ln2lne(L)) == kn3now ) then   ! regular 1D links
                        KA = PROF1D(1,L)
                        KB = PROF1D(2,L)
                        IF (KA < 0 .AND. KB < 0 ) THEN        ! for which profile pointering exists
                            ALFA    = PROF1D(3,L)
                            ZA      = profiles1D(-KA)%ZMIN
                            ZB      = profiles1D(-KB)%ZMIN
                            WA      = profiles1D(-KA)%width
                            WB      = profiles1D(-KB)%width
                            zuL     = (1d0-ALFA)*ZA + ALFA*ZB  ! z on these links
                            wuL     = 1d0 ! (1d0-ALFA)*WA + ALFA*WB  ! z on these links
                            LL      = abs( ln2lne(L) )
                            k       = kn(1,LL)
                            zkk(k)  = zkk(k) + zul*wuL
                            wkk(k)  = wkk(k) + wuL
                            k       = kn(2,LL)
                            zkk(k)  = zkk(k) + zul*wuL
                            wkk(k)  = wkk(k) + wuL
                        ENDIF
                    endif
                 enddo
                 do k = 1, numk
                    if (zk(k) == dmiss .or. zk(k) == zkuni) then
                       if (wkk(k) .ne. 0) then
                          zk(k) = zkk(k) / wkk(k)
                       endif
                    endif
                 enddo
              enddo
              deallocate (zkk, wkk)
          ENDIF

       endif   ! if ( Lnx1D.gt.0 ) then
    endif   ! if (nproflocs > 0) then

!   parallel: reduce nonlin
    if (jampi.eq.1) then
       call reduce_key(nonlin)
    end if

    call restoresam()
    deallocate(XLLIN, XLSAM)
    if ( Lnx1D.gt.0 ) then
       if ( allocated(NSBR)  ) deallocate(NSBR)
       if ( allocated(KBSAM) ) deallocate(KBSAM)
    end if
    deallocate(xpr, ypr, zpr, npr)

    if ( jampi.eq.1 ) then
       if ( allocated(distsam)  ) deallocate(distsam)
       if ( allocated(iconnsam) ) deallocate(iconnsam)
    end if

 endif

 !call duikerstoprofs()

 end subroutine setprofs1D
