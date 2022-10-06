C
C $Id: ppzbts.f 6930 2017-01-17 12:26:37Z pijl $
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
      FUNCTION PPZBTS (RVAL,NBTS)
C
C The function reference "PPZBTS(RVAL,NBTS)" has the value of the real
C number RVAL with all bits of its fraction except the first NBTS set
C to zero.  The Fortran 77 version is straightforward, but probably a
C bit inefficient; there may be a better way.  If and when Fortran 90
C becomes available, the code can be rewritten in a much more efficient
C way.
C
C If the input value is zero, return a zero.
C
      IF (.NOT.(RVAL.EQ.0.)) GO TO 10001
        PPZBTS=0.
        RETURN
10001 CONTINUE
C
C Otherwise, pick off the sign.
C
      RSGN=SIGN(1.,RVAL)
C
C Set up a temporary containing the absolute value of the real, ...
C
      RTMP=ABS(RVAL)
C
C Zero a counter.
C
      IVAL=0
C
C If the temporary is less than 1/2, use successive multiplies by
C two to make it bigger.
C
      IF (.NOT.(RTMP.LT..5)) GO TO 10002
C
  101   RTMP=RTMP*2.
        IVAL=IVAL-1
        IF (RTMP.LT..5) GO TO 101
C
C If the temporary is greater than or equal to 1, use successive
C divides by two to make it smaller.
C
      GO TO 10003
10002 CONTINUE
      IF (.NOT.(RTMP.GE.1.)) GO TO 10004
C
  102   RTMP=RTMP/2.
        IVAL=IVAL+1
        IF (RTMP.GE.1.) GO TO 102
C
10003 CONTINUE
10004 CONTINUE
C
C Once the temporary is in a known range, zero out its lower bits, put
C it back in a range commensurate with that of the input value, tack
C the sign back on, and return the result as the value of the function.
C
      PPZBTS=RSGN*(RTMP-MOD(RTMP,2.**(-NBTS)))*2.**IVAL
C
C Done.
C
      RETURN
C
      END
