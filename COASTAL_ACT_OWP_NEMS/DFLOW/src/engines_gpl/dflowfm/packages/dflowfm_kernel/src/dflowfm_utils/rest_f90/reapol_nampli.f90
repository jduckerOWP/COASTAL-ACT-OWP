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

! $Id: reapol_nampli.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/rest_f90/reapol_nampli.f90 $

      !> Read polygon file (or cross section/pli file) and store in global polygon.
      !! File should contain Tekal block(s) with two or three columns.
      !! The block names may be used for cross sections.
      !! A dmiss line starts a new polyline without a name. Multiple dmiss lines are skipped.
      SUBROUTINE REAPOL_NAMPLI(MPOL, jadoorladen, janampl, ipli)
      USE M_POLYGON
      use network_data, only: netstat, NETSTAT_CELLS_DIRTY
      USE M_MISSING
      use m_alloc
      use unstruc_messages
      use unstruc_files
      use m_flowparameters, only: ifixedweirscheme

      implicit none
      integer :: mpol
      integer, intent(in)           :: jadoorladen !< Append to existing polygons (intended to read multiple crs files)
      integer, intent(in)           :: janampl     !< Store the pli-name as crosssection name
      integer, intent(inout)        :: ipli

      integer :: i
      integer :: nkol
      integer :: nrow
      integer :: nmiss
      integer :: ierr
      double precision :: xx, yy, zz, dz1, dz2
      double precision :: zcrest,sillup, silldown, crestl,taludl, taludr, veg
      character(len=1) :: weirtype

      CHARACTER(len=5) :: CHARMC
      character(len=64) :: MATR
      character(len=256) :: REC

      if (jadoorladen /= 1) then
        if (.not. allocated(XPL)) allocate(XPL(1), YPL(1), ZPL(1))
        XPL = XYMIS
        YPL = XYMIS
        ZPL = XYMIS
        NPL = 0
        call realloc(nampli,20, keepExisting = .false., fill = ' ')
      end if


      CALL READYY('READING POLYGON / land boundary / CRS-FILE',0d0)
   10 CONTINUE
      READ(MPOL,'(A)',END=999,ERR=888) MATR
      IF (MATR(1:1) .EQ. '*' .or. len_trim(matr) == 0) GOTO 10
      READ(MPOL,'(A)',END = 999) REC
      READ(REC,*,iostat=ierr) NROW, NKOL
      if (ierr /= 0) goto 888
      jaKol45 = 0
      if (nkol < 2) then
        CALL QNERROR('File should contain at least 2 or 3 columns, but got:', ' ', ' ') ! nkol)
        goto 999
      else if (nkol > 3 .and. nkol .le. 8) then
         jaKol45 = 1
      else if (nkol .ge. 9) then
         jaKol45 = 2
      end if
      CALL INCREASEPOL(NPL + NROW + 1, 1) ! previous pols (if any) + 1 dmiss + new polyline

11    ipli = ipli + 1         ! Start reading a new polyline

      if (janampl>0) then
         if (len_trim(matr) > 0) then
            if (ipli>size(nampli)) then
               call realloc(nampli, int(1.2*ipli) + 1, keepexisting = .True.)
            endif
            nampli(ipli) = matr  ! Temporarily store cross section name with polyline
         endif
      endif

      if (npl > 0 .and. nrow > 0) then
          ! Separator element for subsequent named polylines
        npl = npl + 1
        xpl(npl) = dmiss
        ypl(npl) = dmiss
        zpl(npl) = dmiss
      end if

      I = 0
 row: DO
        I = I+1
        if (I > NROW) exit

            nmiss = 0
            do ! Read a single line, or multiple until a NON-dmiss line is found
                READ(MPOL,'(A)',END = 999) REC
                ZZ = DMISS ; dz1 = dmiss; dz2 = dmiss
                if (nkol == 10) then
                    READ(REC,*,iostat=ierr) XX,YY,zcrest, sillup, silldown, crestl, taludl, taludr, veg, weirtype  ! read weir data from Baseline format plus weirtype
                    if (ierr /= 0) goto 777
                    ZZ = zcrest ! dummy value for zz to guarantee that ZPL will be filled
                else if (nkol == 9) then
                    READ(REC,*,iostat=ierr) XX,YY,zcrest, sillup, silldown, crestl, taludl, taludr, veg  ! read weir data from Baseline format
                    if (ierr /= 0) goto 777
                    ZZ = zcrest ! dummy value for zz to guarantee that ZPL will be filled
                else if (nkol == 5) then
                    READ(REC,*,iostat=ierr) XX,YY,ZZ,dz1,dz2
                    if (ierr /= 0) goto 777
                else if (nkol == 4) then
                    READ(REC,*,iostat=ierr) XX,YY,ZZ,dz1
                    if (ierr /= 0) goto 777
                else if (nkol == 3) then
                    READ(REC,*,iostat=ierr) XX,YY,ZZ
                    if (ierr /= 0) goto 777
                else
                    READ(REC,*,iostat=ierr) XX,YY
                    if (ierr /= 0) goto 777
                end if
                IF (XX .NE. dmiss .AND. XX .NE. 999.999d0) exit
                nmiss = nmiss + 1
                I = I+1
                if (I > NROW) exit row ! Last row was also dmiss, now exit outer 'row' loop.
            end do
            if (nmiss > 0) then
                backspace(MPOL) ! Last one was NON-dmiss, preceded by one or more dmiss lines
                NROW = NROW-I+1 ! so reread this in the next polyline loop
                MATR = ' '      ! Fake new Tekal block by decrementing NROW and dummy name in MATR
                goto 11
            else
                NPL = NPL + 1
                XPL(NPL) = XX
                YPL(NPL) = YY
                ZPL(NPL) = ZZ
                if (jakol45 == 1) then
                   IF (.NOT. ALLOCATED(DZL) ) THEN
                      ALLOCATE ( DZL(MAXPOL), DZR(MAXPOL) ) ; DZL = DMISS ; DZR = DMISS
                   ENDIF
                   DZL(NPL) = DZ1
                   DZR(NPL) = DZ2
                else if (jakol45 == 2) then
                   IF (.NOT. ALLOCATED(IWEIRT) ) THEN
                      if( allocated( DZL ) ) deallocate( DZL )
                      if( allocated( DZR ) ) deallocate( DZR )
                      ALLOCATE ( DZL(MAXPOL), DZR(MAXPOL), DCREST(MAXPOL), DTL(MAXPOL), DTR(MAXPOL), DVEG(MAXPOL), IWEIRT(MAXPOL) )
                      IWEIRT = dmiss
                   ENDIF
                   DZL(NPL) = sillup
                   DZR(NPL) = silldown
                   DCREST(NPL) = crestl
                   DTL(NPL) = taludl
                   DTR(NPL) = taludr
                   DVEG(NPL) = veg
                   IWEIRT(NPL) = -999  ! if no weirtype has been specified
                   if (nkol .eq. 10) then
                      if (weirtype .eq. 't' .or. weirtype .eq. 'T') then
                          IWEIRT(NPL) = 1
                      elseif (weirtype .eq. 'v' .or. weirtype .eq. 'V')  then
                          IWEIRT(NPL) = 2
                      endif
                   else if (nkol == 9) then
                      if (ifixedweirscheme == 8) then
                         IWEIRT(NPL) = 1
                      elseif (ifixedweirscheme == 9) then
                         IWEIRT(NPL) = 2
                      endif
                   endif
                endif

            end if
            IF (MOD(NPL,100) .EQ. 0) THEN
               CALL READYY(' ',MIN( 1d0,dble(I)/MAXPOL ) )
            ENDIF

      end do row
      GOTO 10


  999 CONTINUE
      ! If last polyline in file had only dmisses, remove last separator element in xpl.
      if (npl > 0) then
         if (xpl(npl) == dmiss) then
            npl = npl-1
         end if
      end if
      CALL READYY(' ',-1d0)
      call doclose (MPOL)

      RETURN

  888 CALL QNREADERROR('SEARCHING NROWS,NCOLS, BUT GETTING', REC, MPOL)
      CALL READYY(' ',-1d0)
      call doclose (MPOL)
      RETURN

  777 CALL QNREADERROR('READING COORDINATES BUT GETTING',REC,MPOL)
      CALL READYY(' ',-1d0)
      call doclose (MPOL)
      RETURN

      END SUBROUTINE REAPOL_NAMPLI
