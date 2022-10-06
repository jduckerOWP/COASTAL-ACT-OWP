C
C $Id: ppppap.f 6930 2017-01-17 12:26:37Z pijl $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE PPPPAP (XCOP,YCOP,NCOP,NBTS)
C
C This routine may be called to pre-process a polygon that is to be
C used as input to one of the polygon-manipulation routines.  The
C polygon is defined by the points (XCOP(I),YCOP(I)), for I = 1 to
C NCOP.  NBTS is the number of significant bits to be left in the
C fractional parts of the point coordinates; you should probably not
C use a value less than about 10 (?) nor one greater than 24 on a
C machine with 32-bit reals or greater than 48 on a machine with
C 64-bit reals.  For most purposes, NBTS = 18 is probably okay.
C
      DIMENSION XCOP(NCOP),YCOP(NCOP)
C
C Reduce the number of significant bits in each point coordinate to
C NBTS by zeroing the remaining bits.  This is useful in avoiding a
C problem that occurs in the trapezoid-producing routines when there
C are edge segments that are very nearly, but not quite, horizontal.
C
      DO 10001 I=1,NCOP
        XCOP(I)=PPZBTS(XCOP(I),NBTS)
        YCOP(I)=PPZBTS(YCOP(I),NBTS)
10001 CONTINUE
C
C Cull adjacent points points that are identical.  This step is
C probably unnecessary now, as I no longer know of any problem
C caused by adjacent identical points, but it does no harm.
C
      NOUT=1
C
      DO 10002 I=2,NCOP
        IF (.NOT.(XCOP(I).NE.XCOP(I-1).OR.YCOP(I).NE.YCOP(I-1)))
     +  GO TO 10003
          NOUT=NOUT+1
          IF (.NOT.(NOUT.NE.I)) GO TO 10004
            XCOP(NOUT)=XCOP(I)
            YCOP(NOUT)=YCOP(I)
10004     CONTINUE
10003   CONTINUE
10002 CONTINUE
C
      NCOP=NOUT
C
C Done.
C
      RETURN
C
      END
