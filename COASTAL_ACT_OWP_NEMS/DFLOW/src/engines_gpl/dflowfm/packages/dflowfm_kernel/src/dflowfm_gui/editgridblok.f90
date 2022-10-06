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

! $Id: editgridblok.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_gui/editgridblok.f90 $

      SUBROUTINE EDITGRIDBLOK(MODE,NFLD,KEY)
      use m_grid
      use unstruc_colors
      implicit none

      integer :: mode, nfld, key
      integer :: newmode

      integer :: ndraw, nlevel, num, nwhat, numb, mp, np
      COMMON /DRAWTHIS/ ndraw(50)
      COMMON /HELPNOW/ WRDKEY,NLEVEL

      integer :: MB,NB,MB2,NB2,NPT,NPT2,NPUTO,ITYPE
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      CHARACTER TEX*20, WRDKEY*40, FIELDOP*40
      integer :: m1b, n1b, m2b, n2b, ipt, ja, jonce, m, n, nput
      double precision :: xp, yp

      TEX    = ' '//FIELDOP(NFLD)
      WRDKEY = FIELDOP(NFLD)
      NLEVEL = 3
      NUM    = 0
      NWHAT  = 0
      NUMB   = 8
      MP     = 0
      NP     = 0
      ITYPE  = 2
      jonce  = 0

      NPUT   = 8
      CALL RESETB(NPUT)
      CALL BOTLIN(0,NUMB,KEY)

    10 CONTINUE
      CALL DRAWNU(KEY)
      CALL TEKB(Xc,Yc,MMAX,NMAX,NCOLLN)
      CALL KTEXT(TEX,1,2,15)
      CALL KTEXT(' Indicate a Block   ',1,3,15)

      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)
      IF (KEY .NE. 23) JONCE = 0

      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            CALL TEKB(Xc,Yc,MMAX,NMAX,0)
            RETURN
         ELSE
            CALL CHOICES(MODE,NUM,NWHAT,KEY)
         ENDIF
      ELSE IF (KEY >= 577) THEN ! Alt+letter switches edit mode.
        call selecteditmode(newmode, key)
        if (newmode > 0 .and. newmode /= mode) then
            mode = newmode
            return
        end if
      ELSE IF (KEY .EQ. 21) THEN
!        INS KEY
!        kijken welk punt
         CALL ISPOIN(     Xc,     Yc,     mmax, nmax, MC,     NC,   Zc, &
                          XP,     YP,     MP,     NP)
         IF (MP .NE. 0) THEN
            IF (NPUT .EQ. 16) THEN
               MB(IPT) = MP
               NB(IPT) = NP
               CALL CIRR(Xc(MP,NP), Yc(MP,NP), NCOLLN)
               IF (NPT .EQ. 1) NPUT = 9
               IF (NPT .EQ. 2) NPUT = 17
               IF (NPT .EQ. 3) NPUT = 18
               IF (NPT .EQ. 4) NPUT = 19
            ELSE
               CALL NEWBLOCKPOINT(MP,NP,JA,IPT)
               IF (JA .EQ. 1) THEN
!                 voeg punt toe
                  CALL SAVEB(NPUT)
                  NPT = NPT + 1
                  MB(NPT) = MP
                  NB(NPT) = NP
                  CALL CIRR(Xc(MB(NPT),NB(NPT)),Yc(MB(NPT),NB(NPT)),NCOLLN)
                  IF (NPT .EQ. 1) NPUT = 9
                  IF (NPT .EQ. 2) NPUT = 17
                  IF (NPT .EQ. 3) NPUT = 18
                  IF (NPT .EQ. 4) NPUT = 19
               ELSE IF (JA .EQ. -1) THEN
!                 niet meer toevoegen
                  CALL QNERROR('4 POINTS: CONTINUE = RIGHT MOUSE OR', 'Enter,',' ')
               ELSE IF (JA .EQ. 0) THEN
!                 oud punt geclickt; uitgummen
                  CALL SAVEB(NPUT)
                  CALL CIRR(Xc(MB(IPT),NB(IPT)),Yc(MB(IPT),NB(IPT)),0)
                  IF (IPT .LE. 2) CALL TEKB(Xc,Yc,MMAX,NMAX,0)
                  MB(IPT) = 0
                  NB(IPT) = 0
                  NPUT    = 16
               ENDIF
            ENDIF
         ENDIF
      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY
         IF (NPT .LE. 1) THEN
           CALL QNERROR('FIRST PRESS MORE POINTS WITH LEFT MOUSE BUTTON',' ',' ')
         ELSE
            CALL TEKB(Xc,Yc,MMAX,NMAX,0)
            CALL POSITIVEBLOK()
            M1B = MAX(MB(3)-1,1)
            N1B = MAX(NB(3)-1,1)
            M2B = MIN(MB(4)+2,MC)
            N2B = MIN(NB(4)+2,NC)
            CALL TEKGRD(      Xc,     Yc,    mmax, nmax, M1B,           &
                             N1B,    M2B,    N2B,0,NDRAW(38), key, mc)
            if (allocated(xch)) then
               CALL TEKGRD(     Xch,    Ych,    mmax, nmax, M1B,           &
                                N1B,    M2B,    N2B,0,NDRAW(16), key, mc)
            end if

!           Begin Operatie
            CALL SAVEgrd()
            IF (NFLD .EQ. 14) THEN
               CALL NULFIELD(Xc,Yc, mmax, nmax)
            ELSE IF (NFLD .EQ. 15) THEN
               CALL CUTFIELD(Xc,Yc,mmax, nmax, MC,NC)
            ELSE IF (NFLD .EQ. 16) THEN
               !CALL ORTHO(X, Y, MB(3), NB(3), MB(4), NB(4), MC, NC, NUM, MMAX,NMAX)!!!
               CALL ORTHOGRID(MB(3), NB(3), MB(4), NB(4))
            ELSE IF (NFLD .EQ. 17) THEN
               CALL DOSMOOTH(NFLD) !Xc,Yc,mmax, nmax, MC,NC,NFLD,IJC,IJYES)
            ENDIF
!           Einde Operatie
            CALL TEKGRD(     Xc,     Yc,    mmax, nmax, M1B,                  &
                             N1B,    M2B,    N2B, NCOLDG, NDRAW(38), key, mc)
            if (allocated(xch)) then
               CALL TEKGRD(     Xch,    Ych,    mmax, nmax, M1B,                   &
                                N1B,    M2B,    N2B, NCOLRG, NDRAW(16), key, mc)
            end if

            IF (NFLD .EQ. 14) THEN
               IF (MB(3) .EQ. 1 .OR. MB(4) .EQ. MC .OR.    &
                   NB(3) .EQ. 1 .OR. NB(4) .EQ. NC    ) THEN
                   CALL ADJUST(Xc, Yc, mmax, nmax, MC, NC)
               ENDIF
            ELSE IF (NFLD .EQ. 15) THEN
                CALL ADJUST(Xc, Yc, mmax, nmax, MC, NC)
                KEY = 3
            ENDIF
            CALL RESETB(NPUT)
            NPUT = 8
         ENDIF
      ELSE IF (KEY .EQ. 23) THEN
!        ESC
         JONCE = JONCE + 1
         IF (JONCE .EQ. 1) THEN
            CALL RESTOREB(NPUT)
         ELSE IF (JONCE .EQ. 2) THEN
            NPUT = 10
            CALL RESETB(NPUT)
         ELSE IF (JONCE .EQ. 3) THEN
            CALL RESTOREgrd()
         ENDIF
         KEY = 3
      ELSE IF (KEY .EQ. 27) THEN
!        TAB
         CALL SHWXYZ(Xc,Yc,Zc,mmax, nmax, MC,NC,0,KEY,M,N)
      ENDIF
!
      GOTO 10
!
      END subroutine editgridblok
