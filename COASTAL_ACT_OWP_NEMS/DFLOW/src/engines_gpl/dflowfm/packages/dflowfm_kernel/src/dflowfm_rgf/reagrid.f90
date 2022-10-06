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

! $Id: reagrid.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_rgf/reagrid.f90 $

      !> Read a curvilinear grid to (ascii) grd-file.
      !! NOTE: reads 'old' (RGFGrid 3.x) and 'new' format (RGFGrid 4.x)
      !!
      !! Format:
      !! Start with at least one comment line, prefixed by '*', with optional keyword Spherical (old RGFGRID 3.x style)
      !! Next, zero or more key=value lines (all optional):
      !!   Coordinate System=Spherical
      !!   Missing Value=...
      !! First subsequent line without a '=' should be '0 0 0' (backwards compatibility)
      !! Next line should be mmax, nmax
      !! That ends the header, start reading coordinates in the usual fashion.
      SUBROUTINE REAgrid(Mrgf, FILNAM, ja)
      USE M_SFERIC
      USE m_grid
      USE M_MISSING, notinuse => xymis ! AvD: temp
      use m_netw  ! vanwege zkuni
      use unstruc_model
      USE M_ARCINFO

      implicit none

      CHARACTER (LEN=*)  :: FILNAM
      INTEGER            :: MRGF, JA
      double precision   :: xymis

      integer :: key
      integer :: Mbnd, mbca, mobs, mout

      CHARACTER NAME2*76, TEX*3, REC*132, REC1*132

      integer :: IPNT, mdep, merr, npart, l, k, ja2, i, j, istat

      logical :: jawel, kw_found

      ja       = 0
      JSFERIC  = 0
      JSFERTEK = 0

      MERR  = 0
      MC    = 0

      xymis = 0d0 ! this is the default for this file type

      read(mrgf,'(a)',iostat=istat) rec
      if (istat > 0) goto 888
      if (istat < 0) goto 9999
      !
      ! Backwards compatible: first line could contain spherical keyword
      if (index(rec, 'Spherical') >= 1  .or. &
          index(rec, 'SPHERICAL') >= 1  ) then
          ! grid has spherical coordinates.
          jsferic=1
      endif
      !
      ! looping keyword records, excluding comment lines
      !
      do
        kw_found = .false.
        read(mrgf,'(a)',iostat=istat) rec
        if (istat > 0) goto 888
        if (istat < 0) goto 9999
        if (rec(1:1) == '*') cycle
        !
        if (index(rec,'Coordinate System') >= 1) then
            kw_found = .true.
            i = index(rec,'=') + 1
            if (index(rec(i:), 'Spherical') >= 1) then
                ! grid has spherical coordinates, overruling previous choices.
                jsferic = 1
            endif
        endif
        !
        if (index(rec,'Missing Value') >= 1) then
            kw_found = .true.
            i = index(rec,'=') + 1
            read(rec(i:),*,iostat=istat) xymis
            if (istat > 0) goto 888
            if (istat < 0) goto 9999
        endif
        !
        if (.not. kw_found) then
            if (index(rec,'=') >= 1) kw_found = .true.
        endif
        !
        if (kw_found) then
            cycle ! read next record from file
        else
            exit  ! record contains the dimensions
        endif
    enddo
    !
    ! End loop, keywords
    !
    !
    ! First record behind the keywords contains dimension of the grid
    !
    read(rec,*,iostat=istat)  mc, nc
    !
    read(mrgf,'(a)',iostat=istat) rec ! read three zero's
    if (istat > 0) goto 888
    if (istat < 0) goto 9999
    !
    !  end read header of rgf-file
    !

      CALL READYY('Reading Grid-File',0d0)


      CALL INCREASEGRID(MC,NC)


      zc = zkuni


      CALL ECRREA(Xc,MMAX,NMAX,MC,NC,MRGF,0d0)
      CALL ECRREA(Yc,MMAX,NMAX,MC,NC,MRGF,0.5d0)

      ! Set to system-wide dxymiss where necessary.
      do i=1,mc
        do j=1,nc
            if (xc(i,j) == xymis .and. yc(i,j) == xymis) then
                xc(i,j) = dxymis
                yc(i,j) = dxymis
            end if
        end do
      end do

      call isitu()

      IPNT  = INDEX(FILNAM,'.')                     ! NOW READ *.DEP FILE, ONLY IF ORGANISED IN DEPTH POINTS
      NAME2 = FILNAM
      WRITE(NAME2(IPNT+1:),'(A)') 'DEP'
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(MDEP,NAME2)
         CALL REAMDD(MDEP,Zc,MC+1,NC+1,JA2)
         do i = 1,mc
            do j = 1,nc
               if (zc(i,j) .ne. dmiss) then
                  zc(i,j) = -1d0*zc(i,j)
               endif
            enddo
         enddo
      ENDIF

      WRITE(NAME2(IPNT+1:),'(A)') 'ASC'
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(MDEP,NAME2)
         call savepol() ! we do not want to use the selecting polygon
         call delpol()
         CALL REAARC(MDEP,0)
         call restorepol()
         if ( ubound(d,1).ge.MC .and. ubound(d,2).ge.NC ) then
            do i = 1,mc
               do j = 1,nc
                  zc(i,j) = d(i,j)
               enddo
            enddo
         end if
      ENDIF

      WRITE(NAME2(IPNT+1:),'(A)') 'bottom'
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(MDEP,NAME2)
         CALL REABOT(MDEP,JA2)
      ENDIF

      WRITE(NAME2(IPNT+1:),'(A)') 'weirs'
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(MDEP,NAME2)
         CALL REAweir(MDEP,JA2)
      ENDIF

      WRITE(NAME2(IPNT+1:),'(A)') 'crs'
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(MDEP,NAME2)
         CALL REAcrs(MDEP,JA2)
      ENDIF

      WRITE(NAME2(IPNT+1:),'(A)') 'bnd'             ! also read *.bnd file and create polygonfile
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(Mbnd,NAME2)

         WRITE(NAME2(IPNT+1:),'(A)') 'bca'          ! also read *.bca file and make *.cmp files
         INQUIRE(FILE=NAME2, EXIST=JAWEL)
         mbca = 0
         if (jawel) then
            CALL OLDFIL(Mbca,NAME2)
         endif

         call reabnd2pol(mbnd,mbca) ! Old, model-specific. TODO: remove or generalize

      ENDIF

      WRITE(NAME2(IPNT+1:),'(A)') 'obs'             ! also read *.obs file and create obsxyn
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(Mobs,NAME2)
         WRITE(NAME2(IPNT+1:),'(A)') '_obs.xyn'
         call newfil(mout,name2)
         call reaobs2stat(mobs, mout)
      endif

      WRITE(NAME2(IPNT+1:),'(A)') 'thd'             ! also read *.thd file and create polygonfile
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(Mbnd,NAME2)

         WRITE(NAME2(IPNT:),'(A)') '_thd.pli'
         call newfil(mout,name2)

         call reathd2pli(mbnd,mout)

      ENDIF

      name2=filnam
      WRITE(NAME2(IPNT+1:),'(A)') 'mnbar'             ! also read *.thd file and create polygonfile
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(Mbnd,NAME2)

         WRITE(NAME2(IPNT:),'(A)') '_bar.pli'

         call newfil(mout,name2)

         call reabar2pli(mbnd,mout)

      ENDIF

      name2=filnam
      WRITE(NAME2(IPNT+1:),'(A)') 'dry'             ! also read *.thd file and create polygonfile
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(Mbnd,NAME2)

         WRITE(NAME2(IPNT:),'(A)') '_dry.pli'

         call newfil(mout,name2)

         call readry2pli(mbnd,mout)

      ENDIF

      CALL READYY(' ',-1d0)
      CALL DOCLOSE (MRGF)

      ! call gridtonet()

      ! call delgrd(key,1,0)


      ja = 1
      return

 9999 CONTINUE
      CALL READYY(' ',-1d0)
      CALL DOCLOSE (MRGF)
      RETURN

  888 CONTINUE
      CALL QNERROR('Reading Error, Try UX2DOS or DOS2UX',  ' ',' ')
      CALL READYY(' ',-1d0)
      CALL DOCLOSE (MRGF)
      RETURN

      end subroutine REAgrid
