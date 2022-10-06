C
C $Id: ppunpo.f 6930 2017-01-17 12:26:37Z pijl $
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
      SUBROUTINE PPUNPO (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,
     +                        RWRK,IWRK,NWRK,URPP,IERR)
C
      DIMENSION XCCP(NCCP),YCCP(NCCP)
      DIMENSION XCSP(NCSP),YCSP(NCSP)
      DIMENSION RWRK(NWRK),IWRK(NWRK)
C
C The subroutine PPUNPO, given X/Y coordinates defining the vertices
C of a "clip polygon" in (XCCP(I),I=1,NCCP) and (YCCP(I),I=1,NCCP),
C X/Y coordinates defining the vertices of a "subject polygon" in
C (XCSP(I),I=1,NCSP) and (YCSP(I),I=1,NCSP), and the real and integer
C workspaces RWRK and IWRK, each of which is of length NWRK, generates
C a set of polygons representing the union of the two input polygons and
C delivers each of them to a user-defined polygon-processing routine
C called URPP.  Errors, in general, result in an immediate RETURN with
C IERR non-zero; on a normal return, IERR is zero.
C
C For most efficient use of memory, IWRK and RWRK should be EQUIVALENCEd
C to each other.
C
C The algorithm used is that described by Bala R. Vatti in the article
C "A Generic Solution to Polygon Clipping", which was published in the
C July, 1992, issue of "Communications of the ACM" (Vol. 35, No. 7).
C
C The various linked lists used in Vatti's algorithm are implemented as
C follows:
C
C LMT (Local Minimum Table).  Formed initially at the lower end of the
C workspace.  Released 3-word nodes are put on a garbage list and may
C be re-used as part of an output polygon.  LMT nodes have the following
C structure:
C
C   0: Y value of a local minimum on one of the two input polygons.
C      LMT nodes are sorted by increasing value of this element.
C
C   1: Index of local minimum (1 to LCCP for clip polygon, LCCP+1 to
C      LCCP+LCSP for subject polygon).
C
C   2: Index of the next node of the LMT.
C
C AET (Active Edge Table).  Occupies space at the lower end of the
C workspace.  Released 10-word nodes are put on a garbage list and may
C be re-used for new AET nodes.  AET nodes have the following structure:
C
C   0: X coordinate at the current scanbeam position.  AET nodes are
C      sorted by increasing value of this element.
C
C   1: X coordinate at the end of the edge segment.  (I added this to
C      get around a problem which arose because Vatti's formulation did
C      not result in correct X coordinates at the end of a segment.)
C
C   2: Y coordinate at the end of the edge segment.
C
C   3: Change in X for a unit increase in Y.
C
C   4: Clip/subject edge flag (0 for clip, 1 for subject).
C
C   5: Left/right flag (0 for left, 1 for right).
C
C   6: Pointer to the next edge in the AET.
C
C   7: Pointer to the previous edge in the AET.
C
C   8: Pointer to the edge segment which succeeds this one.  This value
C      is either positive or negative and has absolute value "n".  If
C      the value is positive, it implies that the indices of the points
C      at the ends of the succeeding edge are "n" and "n+1"; if the
C      value is negative, the indices are "n" and "n-1".  The indices
C      are into the arrays XCCP and YCCP, if element 4 is zero, or XCSP
C      and YCSP, if element 4 is non-zero.
C
C   9: Pointer to output polygon to which the edge is "contributing"
C      (0 if no such polygon).
C
C Output Polygon.  Occupies space at the upper end of the workspace.
C Released 3-word nodes are put on a garbage list from which they can
C be re-used for other polygons.  Output-polygon nodes have the
C following structure:
C
C   Principal Node:
C
C   0: Pointer to the left-end subsidiary node.
C
C   1: Pointer to the right-end subsidiary node.
C
C   2: Pointer to the principal node of the next polygon (0 if none).
C
C   Subsidiary Node:
C
C   0: X coordinate of a point.
C
C   1: Y coordinate of a point.
C
C   2: Pointer to the next subsidiary node to the "right" along the
C      polygon.  ("Left" and "right" are defined from the standpoint
C      of an observer standing on the edge of the polygon and facing
C      inwards.)
C
C SET (Sorted Edge Table).  Occupies space at the lower end of the
C workspace, following the AET.  All space used is reclaimed.  SET
C nodes have the following structure:
C
C   0: X coordinate of edge's intersection with the top of the scanbeam.
C      SET nodes are sorted by decreasing value of this element.
C
C   1: Pointer to a node in the AET.  Says which edge is represented by
C      the node.
C
C   2: Pointer to the next node in the SET.
C
C INT (INtersection Table).  Occupies space at the lower end of the
C workspace, following the AET.  All space used is reclaimed.  INT
C nodes have the following structure:
C
C   0: X coordinate of point of intersection.
C
C   1: Y coordinate of point of intersection.  INT nodes are sorted
C      by increasing value of this element.
C
C   2: Pointer to a node in the AET, identifying one of the two edges
C      that intersect.
C
C   3: Pointer to a later node in the AET, identifying the other edge.
C
C   4: Pointer to the next node in the INT.
C
C Define RBIG to be a large real number.
C
      DATA RBIG / 1.E36 /
C
C Zero error flag.
C
      IERR=0
C
C Decide what the real lengths of the polygons are (depending on whether
C the first point is repeated at the end or not).
C
      LCCP=NCCP
      IF (XCCP(NCCP).EQ.XCCP(1).AND.YCCP(NCCP).EQ.YCCP(1)) LCCP=NCCP-1
C
      LCSP=NCSP
      IF (XCSP(NCSP).EQ.XCSP(1).AND.YCSP(NCSP).EQ.YCSP(1)) LCSP=NCSP-1
C
C Do some simple checks for degenerate cases.
C
      IF (.NOT.(LCCP.LT.3)) GO TO 10001
        GO TO 10003
10001 CONTINUE
C
      IF (.NOT.(LCSP.LT.3)) GO TO 10004
        GO TO 10006
10004 CONTINUE
C
C Initialize the garbage lists, onto which released 3-word and 10-word
C nodes are put for possible re-use.
C
      IG03=0
      IG10=0
C
C Initialize pointers to the last-used elements at the beginning and
C end of the available workspace.  Initially, the whole thing is
C available:
C
      IPWL=0
      IPWU=NWRK+1
C
C Build the "LMT" ("Local Minimum Table").  Initially, it is empty:
C
      ILMT=0
C
C Search for local minima of the clip polygon.  First, find a starting
C place where the Y coordinate changes one way or the other.
C
      INXT=0
C
      DO 10007 I=1,LCCP-1
        IF (.NOT.(YCCP(I).NE.YCCP(I+1))) GO TO 10008
          INXT=I
          YNXT=YCCP(INXT)
          GO TO 101
10008   CONTINUE
10007 CONTINUE
C
C If there is no such starting place, take an error exit.
C
      GO TO 10003
C
C Otherwise, go through the entire polygon from the starting position,
C finding all those places where the Y value increases after having
C decreased.  Each such place constitutes one of the local minima in
C the LMT.
C
  101 IDIR=0
C
      DO 10010 I=0,LCCP
        ILST=INXT
        YLST=YNXT
        INXT=INXT+1
        IF (INXT.GT.LCCP) INXT=INXT-LCCP
        YNXT=YCCP(INXT)
        IF (.NOT.(YNXT.LT.YLST)) GO TO 10011
          IDIR=-1
        GO TO 10012
10011   CONTINUE
        IF (.NOT.(YNXT.GT.YLST)) GO TO 10013
          IF (.NOT.(IDIR.LT.0)) GO TO 10014
            ILMN=IPWL+1
            IPWL=IPWL+3
            IF (.NOT.(IPWL.GE.IPWU)) GO TO 10015
              GO TO 10017
10015       CONTINUE
            RWRK(ILMN)=YLST
            IWRK(ILMN+1)=ILST
            ITM1=0
            ITM2=ILMT
10018       CONTINUE
              IF (ITM2.EQ.0) GO TO 10019
              IF (RWRK(ILMN).LE.RWRK(ITM2)) GO TO 10019
              ITM1=ITM2
              ITM2=IWRK(ITM2+2)
            GO TO 10018
10019       CONTINUE
            IF (.NOT.(ITM1.EQ.0)) GO TO 10020
              ILMT=ILMN
            GO TO 10021
10020       CONTINUE
              IWRK(ITM1+2)=ILMN
10021       CONTINUE
            IWRK(ILMN+2)=ITM2
10014     CONTINUE
          IDIR=+1
10012   CONTINUE
10013   CONTINUE
10010 CONTINUE
C
C In the same way, search for local minima of the subject polygon.
C
      INXT=0
C
      DO 10022 I=1,LCSP-1
        IF (.NOT.(YCSP(I).NE.YCSP(I+1))) GO TO 10023
          INXT=I
          YNXT=YCSP(INXT)
          GO TO 102
10023   CONTINUE
10022 CONTINUE
C
      GO TO 10006
C
  102 IDIR=0
C
      DO 10025 I=0,LCSP
        ILST=INXT
        YLST=YNXT
        INXT=INXT+1
        IF (INXT.GT.LCSP) INXT=INXT-LCSP
        YNXT=YCSP(INXT)
        IF (.NOT.(YNXT.LT.YLST)) GO TO 10026
          IDIR=-1
        GO TO 10027
10026   CONTINUE
        IF (.NOT.(YNXT.GT.YLST)) GO TO 10028
          IF (.NOT.(IDIR.LT.0)) GO TO 10029
            ILMN=IPWL+1
            IPWL=IPWL+3
            IF (.NOT.(IPWL.GE.IPWU)) GO TO 10030
              GO TO 10017
10030       CONTINUE
            RWRK(ILMN)=YLST
            IWRK(ILMN+1)=LCCP+ILST
            ITM1=0
            ITM2=ILMT
10032       CONTINUE
              IF (ITM2.EQ.0) GO TO 10033
              IF (RWRK(ILMN).LE.RWRK(ITM2)) GO TO 10033
              ITM1=ITM2
              ITM2=IWRK(ITM2+2)
            GO TO 10032
10033       CONTINUE
            IF (.NOT.(ITM1.EQ.0)) GO TO 10034
              ILMT=ILMN
            GO TO 10035
10034       CONTINUE
              IWRK(ITM1+2)=ILMN
10035       CONTINUE
            IWRK(ILMN+2)=ITM2
10029     CONTINUE
          IDIR=+1
10027   CONTINUE
10028   CONTINUE
10025 CONTINUE
C
C Initialize the output polygon list pointer to indicate that no
C polygons have been generated yet:
C
      IPPL=0
C
C Initialize the "AET" ("Active Edge Table") to be empty:
C
      IAET=0
C
C Initialize the variable that normally keeps track of the Y coordinate
C at the top of the current "scanbeam"; the value will be used as the Y
C coordinate at the bottom of the first one.
C
      YTOS=RWRK(ILMT)
C
C Loop through the "scanbeams".
C
10036 CONTINUE
C
C YBOS is the Y coordinate of the bottom of the new scanbeam.
C
        YBOS=YTOS
C
C Loop through those local minima in the LMT having Y coordinate
C YBOS; for each, add to the AET the pair of edges that start at
C that local minimum.
C
10037   CONTINUE
C
C Quit if the end of the LMT has been reached.
C
          IF (ILMT.EQ.0) GO TO 10038
C
C Quit if the Y coordinate of the next local minimum is too large.
C
          IF (RWRK(ILMT).GT.YBOS) GO TO 10038
C
C Retrieve in IMIN the index of the coordinates of the local minimum.
C
          IMIN=IWRK(ILMT+1)
C
C Set ICOS to indicate whether the local minimum comes from the clip
C polygon or the subject polygon.  XMIN and YMIN are the X and Y
C coordinates of the local minimum.  ILST indexes the coordinates of
C the last point along the polygon; the coordinates are XLST and YLST.
C Similarly, INXT indexes the coordinates of the next point along
C the polygon; the coordinates are XNXT and YNXT.
C
          IF (.NOT.(IMIN.LE.LCCP)) GO TO 10039
            ICOS=0
            XMIN=XCCP(IMIN)
            YMIN=YCCP(IMIN)
            ILST=IMIN-1
            IF (ILST.LT.1) ILST=ILST+LCCP
            XLST=XCCP(ILST)
            YLST=YCCP(ILST)
            INXT=IMIN+1
            IF (INXT.GT.LCCP) INXT=INXT-LCCP
            XNXT=XCCP(INXT)
            YNXT=YCCP(INXT)
          GO TO 10040
10039     CONTINUE
            ICOS=1
            IMIN=IMIN-LCCP
            XMIN=XCSP(IMIN)
            YMIN=YCSP(IMIN)
            ILST=IMIN-1
            IF (ILST.LT.1) ILST=ILST+LCSP
            XLST=XCSP(ILST)
            YLST=YCSP(ILST)
            INXT=IMIN+1
            IF (INXT.GT.LCSP) INXT=INXT-LCSP
            XNXT=XCSP(INXT)
            YNXT=YCSP(INXT)
10040     CONTINUE
C
C Now we must scan the AET to determine where to put the new edges.
C After executing the loop below, ITM1 will point to the node after
C which they will be inserted (zero if at beginning) and ITM2 will
C point to the node before which they will be inserted (zero if at
C end).  The variable IOCP will be updated to indicate whether the
C local minimum is inside (1) or outside (0) the clip polygon.
C Similarly, IOSP will be updated to indicate whether the local
C minimum is inside (1) or outside (0) the subject polygon.
C
          ITM1=0
          ITM2=IAET
C
          IOCP=0
          IOSP=0
C
10041     CONTINUE
C
C Exit if the end of the AET has been reached.
C
            IF (ITM2.EQ.0) GO TO 10042
C
C Exit if the new local minimum fits between elements ITM1 and ITM2 of
C the AET.
C
            IF (XMIN.LE.RWRK(ITM2)) GO TO 10042
C
C Advance to the next position in the AET.
C
            ITM1=ITM2
            ITM2=IWRK(ITM2+6)
C
C Update the flags that say where we are relative to the clip and
C subject polygons.
C
            IF (.NOT.(IWRK(ITM1+4).EQ.0)) GO TO 10043
              IOCP=1-IOCP
            GO TO 10044
10043       CONTINUE
              IOSP=1-IOSP
10044       CONTINUE
C
C End of loop through the AET.
C
          GO TO 10041
10042     CONTINUE
C
C Create two new nodes in the AET.  Either re-use 10-word nodes from the
C garbage list or create new ones.
C
          IF (.NOT.(IG10.NE.0)) GO TO 10045
            IPNL=IG10
            IG10=IWRK(IG10)
          GO TO 10046
10045     CONTINUE
            IPNL=IPWL+1
            IPWL=IPWL+10
            IF (.NOT.(IPWL.GE.IPWU)) GO TO 10047
              GO TO 10017
10047       CONTINUE
10046     CONTINUE
C
          IF (.NOT.(IG10.NE.0)) GO TO 10049
            IPNN=IG10
            IG10=IWRK(IG10)
          GO TO 10050
10049     CONTINUE
            IPNN=IPWL+1
            IPWL=IPWL+10
            IF (.NOT.(IPWL.GE.IPWU)) GO TO 10051
              GO TO 10017
10051       CONTINUE
10050     CONTINUE
C
C Fill in the information about the two new edges:
C
          RWRK(IPNL)=XMIN
          RWRK(IPNN)=XMIN
C
          RWRK(IPNL+1)=XLST
          RWRK(IPNN+1)=XNXT
C
          RWRK(IPNL+2)=YLST
          RWRK(IPNN+2)=YNXT
C
          IF (.NOT.(YLST.NE.YMIN)) GO TO 10053
            RWRK(IPNL+3)=(XLST-XMIN)/(YLST-YMIN)
          GO TO 10054
10053     CONTINUE
            RWRK(IPNL+3)=SIGN(RBIG,XLST-XMIN)
10054     CONTINUE
C
          IF (.NOT.(YNXT.NE.YMIN)) GO TO 10055
            RWRK(IPNN+3)=(XNXT-XMIN)/(YNXT-YMIN)
          GO TO 10056
10055     CONTINUE
            RWRK(IPNN+3)=SIGN(RBIG,XNXT-XMIN)
10056     CONTINUE
C
          IWRK(IPNL+4)=ICOS
          IWRK(IPNN+4)=ICOS
C
          IF (.NOT.(ICOS.EQ.0)) GO TO 10057
            IOPO=IOCP
          GO TO 10058
10057     CONTINUE
            IOPO=IOSP
10058     CONTINUE
C
          IF (.NOT.(RWRK(IPNL+3).LT.RWRK(IPNN+3))) GO TO 10059
C
            IPE1=IPNL
            IPE2=IPNN
C
          GO TO 10060
10059     CONTINUE
C
            IPE1=IPNN
            IPE2=IPNL
C
10060     CONTINUE
C
          IWRK(IPE1+5)=IOPO
          IWRK(IPE2+5)=1-IOPO
C
          IF (.NOT.(ITM1.EQ.0)) GO TO 10061
            IAET=IPE1
          GO TO 10062
10061     CONTINUE
            IWRK(ITM1+6)=IPE1
10062     CONTINUE
C
          IWRK(IPE1+6)=IPE2
          IWRK(IPE2+6)=ITM2
          IF (ITM2.NE.0) IWRK(ITM2+7)=IPE2
          IWRK(IPE2+7)=IPE1
          IWRK(IPE1+7)=ITM1
C
          IWRK(IPNL+8)=-ILST
          IWRK(IPNN+8)=+INXT
C
C If the edges are "contributing", create an output polygon for them
C to "contribute" to and put the initial point in it; otherwise, zero
C the output-polygon pointers.
C
          IF (.NOT.((IOCP.EQ.0.AND.IOSP.EQ.0).OR.(IOCP.NE.0.AND.IOSP.EQ.
     +0.AND.ICOS.EQ.0).OR.(IOCP.EQ.0.AND.IOSP.NE.0.AND.ICOS.NE.0)))
     +    GO TO 10063
C
            IF (.NOT.(IG03.NE.0)) GO TO 10064
              IPSN=IG03
              IG03=IWRK(IG03)
            GO TO 10065
10064       CONTINUE
              IPWU=IPWU-3
              IF (.NOT.(IPWU.LE.IPWL)) GO TO 10066
                GO TO 10017
10066         CONTINUE
              IPSN=IPWU
10065       CONTINUE
C
            RWRK(IPSN  )=XMIN
            RWRK(IPSN+1)=YMIN
            IWRK(IPSN+2)=0
C
            IF (.NOT.(IG03.NE.0)) GO TO 10068
              IPPN=IG03
              IG03=IWRK(IG03)
            GO TO 10069
10068       CONTINUE
              IPWU=IPWU-3
              IF (.NOT.(IPWU.LE.IPWL)) GO TO 10070
                GO TO 10017
10070         CONTINUE
              IPPN=IPWU
10069       CONTINUE
C
            IWRK(IPPN  )=IPSN
            IWRK(IPPN+1)=IPSN
            IWRK(IPPN+2)=IPPL
C
            IPPL=IPPN
            IWRK(IPNL+9)=IPPN
            IWRK(IPNN+9)=IPPN
C
          GO TO 10072
10063     CONTINUE
C
            IWRK(IPNL+9)=0
            IWRK(IPNN+9)=0
C
10072     CONTINUE
C
C Put the current LMT node on the appropriate garbage list for re-use.
C
          IWRK(ILMT)=IG03
          IG03=ILMT
C
C Advance to the next element of the LMT.
C
          ILMT=IWRK(ILMT+2)
C
C End of the loop through the LMT.
C
        GO TO 10037
10038   CONTINUE
C
C At this point, if the AET is empty, the scanbeam loop is exited.
C
  103 CONTINUE
        IF (IAET.EQ.0) GO TO 10073
C
C Scan the AET to compute the value of the Y coordinate at the top of
C the scanbeam (YTOS) and to look for horizontal edges in the list.
C
        ITMP=IAET
C
        YTOS=RWRK(ITMP+2)
C
        IF (ILMT.NE.0) YTOS=MIN(YTOS,RWRK(ILMT))
C
10074   CONTINUE
C
C Check for a horizontal section.
C
          IF (.NOT.(YTOS.EQ.YBOS)) GO TO 10075
C
C Step through points in the user's arrays until the end of the
C horizontal section is reached, updating the X coordinate and the
C index of the successor edge as we go.
C
            INNP=ABS(IWRK(ITMP+8))
C
10076       CONTINUE
C
              IF (.NOT.(IWRK(ITMP+4).EQ.0)) GO TO 10077
                IF (.NOT.(INNP.LT.1)) GO TO 10078
                  INNP=INNP+LCCP
                GO TO 10079
10078           CONTINUE
                IF (.NOT.(INNP.GT.LCCP)) GO TO 10080
                  INNP=INNP-LCCP
10079           CONTINUE
10080           CONTINUE
                IF (YCCP(INNP).NE.YBOS) GO TO 10081
                RWRK(ITMP)=XCCP(INNP)
              GO TO 10082
10077         CONTINUE
                IF (.NOT.(INNP.LT.1)) GO TO 10083
                  INNP=INNP+LCSP
                GO TO 10084
10083           CONTINUE
                IF (.NOT.(INNP.GT.LCSP)) GO TO 10085
                  INNP=INNP-LCSP
10084           CONTINUE
10085           CONTINUE
                IF (YCSP(INNP).NE.YBOS) GO TO 10081
                RWRK(ITMP)=XCSP(INNP)
10082         CONTINUE
C
              RWRK(ITMP+1)=RWRK(ITMP)
C
              IWRK(ITMP+8)=SIGN(INNP,IWRK(ITMP+8))
              INNP=INNP+SIGN(1,IWRK(ITMP+8))
C
            GO TO 10076
10081       CONTINUE
C
C Compute a quantity that will be used to recognize the successor of
C the horizontal edge.
C
            INNL=ABS(IWRK(ITMP+8))-SIGN(1,IWRK(ITMP+8))
            IF (.NOT.(INNL.LE.0)) GO TO 10086
              IF (.NOT.(IWRK(ITMP+4).EQ.0)) GO TO 10087
                INNL=INNL+LCCP
              GO TO 10088
10087         CONTINUE
                INNL=INNL+LCSP
10088         CONTINUE
10086       CONTINUE
            INNL=-SIGN(INNL,IWRK(ITMP+8))
C
C Zero the pointer to the list of intersection points.
C
            IINT=0
C
C Save the current value of the pointer to the last word currently used
C in the lower end of the workspace, so that the space occupied by the
C list of intersection points can easily be reclaimed.
C
            ISWL=IPWL
C
C Initialize pointers used below.  The horizontal edge is considered
C to intersect edges that it actually passes over.  If there are edges
C in the AET with X coordinates equal to the X coordinate of the end of
C the horizontal edge, it only intersects them if that is necessary in
C order to make it and its successor be next to each other in the AET.
C
            IINN=-1
            IINQ=0
C
C Generate the list of intersection points, either to the left ...
C
            IF (.NOT.(IWRK(ITMP+7).NE.0)) GO TO 10089
C
              IDUM=IWRK(ITMP+7)
C
10090         CONTINUE
C
                IF (RWRK(IDUM).LT.RWRK(ITMP)) GO TO 10091
C
                IF (.NOT.(IWRK(IDUM+4).EQ.IWRK(ITMP+4).AND.IWRK(IDUM+8).
     +EQ.INNL)) GO TO 10092
                  IINQ=IINN
                  GO TO 10091
10092           CONTINUE
C
                IF (.NOT.(IINT.EQ.0)) GO TO 10093
                  IINT=IPWL+1
                GO TO 10094
10093           CONTINUE
                  IWRK(IINN+4)=IPWL+1
10094           CONTINUE
C
                IINN=IPWL+1
                IPWL=IPWL+5
C
                IF (.NOT.(IPWL.GE.IPWU)) GO TO 10095
                  GO TO 10017
10095           CONTINUE
C
                RWRK(IINN)=RWRK(IDUM)
                RWRK(IINN+1)=YBOS
                IWRK(IINN+2)=IDUM
                IWRK(IINN+3)=ITMP
                IWRK(IINN+4)=0
C
                IF (RWRK(IDUM).GT.RWRK(ITMP)) IINQ=IINN
C
                IDUM=IWRK(IDUM+7)
C
                IF (IDUM.EQ.0) GO TO 10091
C
              GO TO 10090
10091         CONTINUE
C
10089       CONTINUE
C
C ... or to the right.
C
            IF (.NOT.(IINQ.EQ.0)) GO TO 10097
C
              IINT=0
              IPWL=ISWL
              IINN=-1
C
              IF (.NOT.(IWRK(ITMP+6).NE.0)) GO TO 10098
C
                IDUM=IWRK(ITMP+6)
C
10099           CONTINUE
C
                  IF (RWRK(IDUM).GT.RWRK(ITMP)) GO TO 10100
C
                  IF (.NOT.(IWRK(IDUM+4).EQ.IWRK(ITMP+4).AND.IWRK(IDUM+8
     +).EQ.INNL)) GO TO 10101
                    IINQ=IINN
                    GO TO 10100
10101             CONTINUE
C
                  IF (.NOT.(IINT.EQ.0)) GO TO 10102
                    IINT=IPWL+1
                  GO TO 10103
10102             CONTINUE
                    IWRK(IINN+4)=IPWL+1
10103             CONTINUE
C
                  IINN=IPWL+1
                  IPWL=IPWL+5
C
                  IF (.NOT.(IPWL.GE.IPWU)) GO TO 10104
                    GO TO 10017
10104             CONTINUE
C
                  RWRK(IINN)=RWRK(IDUM)
                  RWRK(IINN+1)=YBOS
                  IWRK(IINN+2)=ITMP
                  IWRK(IINN+3)=IDUM
                  IWRK(IINN+4)=0
C
                  IF (RWRK(IDUM).LT.RWRK(ITMP)) IINQ=IINN
C
                  IDUM=IWRK(IDUM+6)
C
                  IF (IDUM.EQ.0) GO TO 10100
C
                GO TO 10099
10100           CONTINUE
C
10098         CONTINUE
C
10097       CONTINUE
C
C Clear entries at the end of the intersection list that don't need to
C be considered to be intersections.  (This may clear the whole list.)
C
            IF (.NOT.(IINQ.EQ.0)) GO TO 10106
              IINT=0
              IPWL=ISWL
            GO TO 10107
10106       CONTINUE
            IF (.NOT.(IINQ.GT.0)) GO TO 10108
              IWRK(IINQ+4)=0
10107       CONTINUE
10108       CONTINUE
C
C If any intersection points were found, process them and then reclaim
C the space used for the list.
C
            IF (.NOT.(IINT.NE.0)) GO TO 10109
              L10111=    1
              GO TO 10111
10110         CONTINUE
              IPWL=ISWL
10109       CONTINUE
C
C The horizontal edge is terminating at this point, so handle that.
C
            L10113=    1
            GO TO 10113
10112       CONTINUE
C
C Go back to see if the AET is empty now and, if not, to rescan it for
C more horizontal segments.
C
            GO TO 103
C
10075     CONTINUE
C
C Move to the next node in the AET.
C
          ITMP=IWRK(ITMP+6)
C
C Quit if there are none.
C
          IF (ITMP.EQ.0) GO TO 10114
C
C Update the variable that says where the top of the scanbeam is.
C
          YTOS=MIN(YTOS,RWRK(ITMP+2))
C
        GO TO 10074
10114   CONTINUE
C
C Create a table of all intersections of edges in the AET, sorted in
C order of increasing Y coordinate.  To do this, we also create a table
C of the current edges in the AET, sorted in the opposite order in which
C they intersect the top of the scanbeam.  Initially, the intersection
C table is empty:
C
        IINT=0
C
C The intersection table and the sorted edge table are formed in the
C lower part of the workspace array.  The value of the pointer to the
C last word currently used in that part of the workspace is saved so
C that, when we are done using the INT and the SET, the space used for
C them can be reclaimed by just restoring the value of this pointer:
C
        ISWL=IPWL
C
C Initialize the "Sorted Edge Table" to contain just the first edge
C from the AET.
C
        ISET=IPWL+1
C
        IPWL=IPWL+3
C
        IF (.NOT.(IPWL.GE.IPWU)) GO TO 10115
          GO TO 10017
10115   CONTINUE
C
        RWRK(ISET)=RWRK(IAET+1)+(YTOS-RWRK(IAET+2))*RWRK(IAET+3)
        IWRK(ISET+1)=IAET
        IWRK(ISET+2)=0
C
C Examine each of the remaining edges in the AET, one at a time,
C looking for intersections with edges that have already gone into
C the SET; for each one found, generate an entry in the INT.  Special
C care is taken to ensure that edges which are each other's successors
C end up adjacent to each other in the AET.
C
        ITMP=IWRK(IAET+6)
C
10117   CONTINUE
C
          IF (ITMP.EQ.0) GO TO 10118
C
          XTMP=RWRK(ITMP+1)+(YTOS-RWRK(ITMP+2))*RWRK(ITMP+3)
C
          IST1=0
          IST2=ISET
C
10119     CONTINUE
C
            IF (IST2.EQ.0) GO TO 10120
            IF (XTMP.GT.RWRK(IST2)) GO TO 10120
C
            IF (.NOT.(XTMP.EQ.RWRK(IST2))) GO TO 10121
C
              IST3=IWRK(IST2+2)
              IST4=0
C
10122         CONTINUE
C
                IF (IST3.EQ.0) GO TO 10123
                IF (XTMP.NE.RWRK(IST3)) GO TO 10123
C
                IF (.NOT.(IWRK(IWRK(IST3+1)+4).EQ.IWRK(ITMP+4).AND.IWRK(
     +IWRK(IST3+1)+8).EQ.-IWRK(ITMP+8))) GO TO 10124
                  IST4=1
                  GO TO 10123
10124           CONTINUE
C
                IST3=IWRK(IST3+2)
C
              GO TO 10122
10123         CONTINUE
C
              IF (IST4.EQ.0) GO TO 10120
C
              XINT=XTMP
              YINT=YTOS
C
            GO TO 10125
10121       CONTINUE
C
              IF (.NOT.(ABS(RWRK(ITMP+3)-RWRK(IWRK(IST2+1)+3)).GT.1.E-6)
     +)       GO TO 10126
                YINT=YBOS-(RWRK(ITMP  )-RWRK(IWRK(IST2+1)  ))/
     +                    (RWRK(ITMP+3)-RWRK(IWRK(IST2+1)+3))
              GO TO 10127
10126         CONTINUE
                YINT=.5*(YBOS+YTOS)
10127         CONTINUE
C
              IF (.NOT.(ABS(RWRK(ITMP+3)).LT.ABS(RWRK(IWRK(IST2+1)+3))))
     +        GO TO 10128
                XINT=RWRK(ITMP+1)+(YINT-RWRK(ITMP+2))*RWRK(ITMP+3)
              GO TO 10129
10128         CONTINUE
                XINT=RWRK(IWRK(IST2+1)+1)+(YINT-RWRK(IWRK(IST2+1)+2))*
     +               RWRK(IWRK(IST2+1)+3)
10129         CONTINUE
C
10125       CONTINUE
C
            IINN=IPWL+1
            IPWL=IPWL+5
C
            IF (.NOT.(IPWL.GE.IPWU)) GO TO 10130
              GO TO 10017
10130       CONTINUE
C
            RWRK(IINN)=XINT
            RWRK(IINN+1)=YINT
            IWRK(IINN+2)=IWRK(IST2+1)
            IWRK(IINN+3)=ITMP
C
            IIN1=0
            IIN2=IINT
C
10132       CONTINUE
              IF (IIN2.EQ.0) GO TO 10133
              IF (RWRK(IINN+1).LE.RWRK(IIN2+1)) GO TO 10133
              IIN1=IIN2
              IIN2=IWRK(IIN2+4)
            GO TO 10132
10133       CONTINUE
C
            IF (.NOT.(IIN1.EQ.0)) GO TO 10134
              IINT=IINN
            GO TO 10135
10134       CONTINUE
              IWRK(IIN1+4)=IINN
10135       CONTINUE
C
            IWRK(IINN+4)=IIN2
C
            IST1=IST2
            IST2=IWRK(IST2+2)
C
          GO TO 10119
10120     CONTINUE
C
          ISTN=IPWL+1
          IPWL=IPWL+3
C
          IF (.NOT.(IPWL.GE.IPWU)) GO TO 10136
            GO TO 10017
10136     CONTINUE
C
          IF (.NOT.(IST1.EQ.0)) GO TO 10138
            ISET=ISTN
          GO TO 10139
10138     CONTINUE
            IWRK(IST1+2)=ISTN
10139     CONTINUE
C
          RWRK(ISTN)=XTMP
          IWRK(ISTN+1)=ITMP
          IWRK(ISTN+2)=IST2
C
          ITMP=IWRK(ITMP+6)
C
        GO TO 10117
10118   CONTINUE
C
C If intersections have been found, process them.
C
        IF (.NOT.(IINT.NE.0)) GO TO 10140
          L10111=    2
          GO TO 10111
10141     CONTINUE
10140   CONTINUE
C
C Discard the intersection table and the sorted edge table.
C
        IPWL=ISWL
C
C Loop through all the edges in the AET, updating the X coordinates and
C further processing those that terminate at the top of the scanbeam.
C
        ITMP=IAET
C
10142   CONTINUE
C
C Exit if all the edges have been done.
C
          IF (ITMP.EQ.0) GO TO 10143
C
C Update the X coordinate to its position at the top of the scanbeam.
C
          RWRK(ITMP)=RWRK(ITMP+1)+(YTOS-RWRK(ITMP+2))*RWRK(ITMP+3)
C
C If the edge terminates at the top of this scanbeam, process it.
C
          IF (.NOT.(RWRK(ITMP+2).EQ.YTOS)) GO TO 10144
            L10113=    2
            GO TO 10113
10145       CONTINUE
10144     CONTINUE
C
C Advance to the next edge in the AET.
C
          ITMP=IWRK(ITMP+6)
C
C End of loop on edges in the AET.
C
        GO TO 10142
10143   CONTINUE
C
C End of scanbeam loop.
C
      GO TO 10036
10073 CONTINUE
C
C Dump out all the polygons that have been formed.
C
C THE FOLLOWING CODE HAS BEEN REPLACED BY CODE THAT CULLS OUT DUPLICATE
C ADJACENT POINTS.  SINCE THE REPLACEMENT CODE IS SLOWER, IT WOULD BE
C ADVANTAGEOUS TO FIGURE OUT (ABOVE) HOW TO PREVENT THE DUPLICATES FROM
C SNEAKING IN.  ONCE THAT HAS BEEN DONE, THE FOLLOWING CODE CAN BE PUT
C BACK IN:
C
C       MXYC=(IPWU-1-IPWL)/2
C       IPXC=IPWL
C       IPYC=IPWL+MXYC
C       WHILE (IPPL.NE.0)
C         NXYC=0
C         ITMP=IWRK(IPPL)
C         WHILE (ITMP.NE.0)
C           NXYC=NXYC+1
C           IF (NXYC.GE.MXYC)
C             INVOKE (WORKSPACE-TOO-SMALL,NR)
C           END IF
C           RWRK(IPXC+NXYC)=RWRK(ITMP)
C           RWRK(IPYC+NXYC)=RWRK(ITMP+1)
C           ITMP=IWRK(ITMP+2)
C         END WHILE
C         NXYC=NXYC+1
C         RWRK(IPXC+NXYC)=RWRK(IWRK(IPPL))
C         RWRK(IPYC+NXYC)=RWRK(IWRK(IPPL)+1)
C         CALL URPP (RWRK(IPXC+1),RWRK(IPYC+1),NXYC)
C         IPPL=IWRK(IPPL+2)
C       END WHILE
C
      MXYC=(IPWU-1-IPWL)/2
      IF (.NOT.(MXYC.LT.1)) GO TO 10146
        GO TO 10017
10146 CONTINUE
      IPXC=IPWL
      IPYC=IPWL+MXYC
10148 CONTINUE
      IF (.NOT.(IPPL.NE.0)) GO TO 10149
        NXYC=1
        ITMP=IWRK(IPPL)
        RWRK(IPXC+1)=RWRK(ITMP  )
        RWRK(IPYC+1)=RWRK(ITMP+1)
        ITMP=IWRK(ITMP+2)
10150   CONTINUE
        IF (.NOT.(ITMP.NE.0)) GO TO 10151
          IF (.NOT.(RWRK(ITMP).NE.RWRK(IPXC+NXYC).OR.RWRK(ITMP+1).NE.RWR
     +K(IPYC+NXYC))) GO TO 10152
            NXYC=NXYC+1
            IF (.NOT.(NXYC.GE.MXYC)) GO TO 10153
              GO TO 10017
10153       CONTINUE
            RWRK(IPXC+NXYC)=RWRK(ITMP)
            RWRK(IPYC+NXYC)=RWRK(ITMP+1)
10152     CONTINUE
          ITMP=IWRK(ITMP+2)
        GO TO 10150
10151   CONTINUE
        IF (.NOT.(RWRK(IPXC+NXYC).NE.RWRK(IPXC+1).OR.RWRK(IPYC+NXYC).NE.
     +RWRK(IPYC+1))) GO TO 10155
          NXYC=NXYC+1
          RWRK(IPXC+NXYC)=RWRK(IPXC+1)
          RWRK(IPYC+NXYC)=RWRK(IPYC+1)
10155   CONTINUE
        IF (NXYC.GE.4) CALL URPP (RWRK(IPXC+1),RWRK(IPYC+1),NXYC)
        IPPL=IWRK(IPPL+2)
      GO TO 10148
10149 CONTINUE
C
C Normal exit.
C
      RETURN
C
C The following internal procedure processes the list of intersection
C points that IINT points to.  On entry, it may be assumed that IINT
C has been verified to be non-zero.
C
10111 CONTINUE
C
C Loop through all the points of intersection.
C
10156   CONTINUE
C
C Extract the coordinates of the point of intersection and the indices
C of the two AET nodes describing the edges that intersected.
C
  201     CONTINUE
C
          XINT=RWRK(IINT)
          YINT=RWRK(IINT+1)
C
          IPE1=IWRK(IINT+2)
          IPE2=IWRK(IINT+3)
C
C If the two edges are not adjacent in the AET, there's a problem.  We
C look for the next intersection of adjacent edges and move it to the
C beginning of the list.
C
          IF (.NOT.(IWRK(IPE1+6).NE.IPE2)) GO TO 10157
C
            IIN1=IINT
            IIN2=IWRK(IINT+4)
C
10158       CONTINUE
C
              IF (.NOT.(IIN2.EQ.0)) GO TO 10159
                IERR=1
                GO TO 10161
10159         CONTINUE
C
              IF (IWRK(IWRK(IIN2+2)+6).EQ.IWRK(IIN2+3)) GO TO 10162
C
              IIN1=IIN2
              IIN2=IWRK(IIN2+4)
C
            GO TO 10158
10162       CONTINUE
C
            IWRK(IIN1+4)=IWRK(IIN2+4)
            IWRK(IIN2+4)=IINT
            IINT=IIN2
C
            GO TO 201
C
10157     CONTINUE
C
C Check whether or not both edges are from the same input polygon.
C
          IF (.NOT.(IWRK(IPE1+4).EQ.IWRK(IPE2+4))) GO TO 10163
C
C Both edges are from the clip polygon or both are from the subject
C polygon.  If edge 1 is contributing to an output polygon, then edge
C 2 should be also, in which case we add the point of intersection to
C the left side of one polygon and to the right side of the other
C polygon.  In either case, we must swap the left/right flags in the
C two edges.
C
            IF (.NOT.(IWRK(IPE1+9).NE.0.OR.IWRK(IPE2+9).NE.0)) GO TO 101
     +64
C
              IF (.NOT.(IWRK(IPE1+9).EQ.0.OR.IWRK(IPE2+9).EQ.0)) GO TO 1
     +0165
                IERR=2
                GO TO 10161
10165         CONTINUE
C
              IF (.NOT.(IG03.NE.0)) GO TO 10167
                IPSN=IG03
                IG03=IWRK(IG03)
              GO TO 10168
10167         CONTINUE
                IPWU=IPWU-3
                IF (.NOT.(IPWU.LE.IPWL)) GO TO 10169
                  GO TO 10017
10169           CONTINUE
                IPSN=IPWU
10168         CONTINUE
C
              RWRK(IPSN  )=XINT
              RWRK(IPSN+1)=YINT
C
              IF (.NOT.(IWRK(IPE1+5).EQ.1)) GO TO 10171
                IWRK(IPSN+2)=IWRK(IWRK(IPE1+9))
                IWRK(IWRK(IPE1+9))=IPSN
              GO TO 10172
10171         CONTINUE
                IWRK(IPSN+2)=0
                IWRK(IWRK(IWRK(IPE1+9)+1)+2)=IPSN
                IWRK(IWRK(IPE1+9)+1)=IPSN
10172         CONTINUE
C
              IF (.NOT.(IG03.NE.0)) GO TO 10173
                IPSN=IG03
                IG03=IWRK(IG03)
              GO TO 10174
10173         CONTINUE
                IPWU=IPWU-3
                IF (.NOT.(IPWU.LE.IPWL)) GO TO 10175
                  GO TO 10017
10175           CONTINUE
                IPSN=IPWU
10174         CONTINUE
C
              RWRK(IPSN  )=XINT
              RWRK(IPSN+1)=YINT
C
              IF (.NOT.(IWRK(IPE2+5).EQ.1)) GO TO 10177
                IWRK(IPSN+2)=IWRK(IWRK(IPE2+9))
                IWRK(IWRK(IPE2+9))=IPSN
              GO TO 10178
10177         CONTINUE
                IWRK(IPSN+2)=0
                IWRK(IWRK(IWRK(IPE2+9)+1)+2)=IPSN
                IWRK(IWRK(IPE2+9)+1)=IPSN
10178         CONTINUE
C
10164       CONTINUE
C
            IDUM=IWRK(IPE1+5)
            IWRK(IPE1+5)=IWRK(IPE2+5)
            IWRK(IPE2+5)=IDUM
C
C One edge is from the clip polygon and the other is from the
C subject polygon.  Check for a local minimum.
C
          GO TO 10179
10163     CONTINUE
          IF (.NOT.((IWRK(IPE1+4).EQ.1.AND.IWRK(IPE1+5).EQ.0.AND.IWRK(IP
     +E2+4).EQ.0.AND.IWRK(IPE2+5).EQ.1).OR.(IWRK(IPE1+4).EQ.0.AND.IWRK(I
     +PE1+5).EQ.0.AND.IWRK(IPE2+4).EQ.1.AND.IWRK(IPE2+5).EQ.1))) GO TO 1
     +0180
C
C Process a local minimum.
C
            IF (.NOT.(IWRK(IPE1+9).NE.0.OR.IWRK(IPE2+9).NE.0)) GO TO 101
     +81
              IERR=3
              GO TO 10161
10181       CONTINUE
C
            IF (.NOT.(IG03.NE.0)) GO TO 10183
              IPSN=IG03
              IG03=IWRK(IG03)
            GO TO 10184
10183       CONTINUE
              IPWU=IPWU-3
              IF (.NOT.(IPWU.LE.IPWL)) GO TO 10185
                GO TO 10017
10185         CONTINUE
              IPSN=IPWU
10184       CONTINUE
C
            RWRK(IPSN  )=XINT
            RWRK(IPSN+1)=YINT
            IWRK(IPSN+2)=0
C
            IF (.NOT.(IG03.NE.0)) GO TO 10187
              IPPN=IG03
              IG03=IWRK(IG03)
            GO TO 10188
10187       CONTINUE
              IPWU=IPWU-3
              IF (.NOT.(IPWU.LE.IPWL)) GO TO 10189
                GO TO 10017
10189         CONTINUE
              IPPN=IPWU
10188       CONTINUE
C
            IWRK(IPPN  )=IPSN
            IWRK(IPPN+1)=IPSN
            IWRK(IPPN+2)=IPPL
            IPPL=IPPN
C
            IWRK(IPE1+9)=IPPN
            IWRK(IPE2+9)=IPPN
C
C Check for a left intersection.
C
          GO TO 10179
10180     CONTINUE
          IF (.NOT.((IWRK(IPE1+4).EQ.0.AND.IWRK(IPE1+5).EQ.0.AND.IWRK(IP
     +E2+4).EQ.1.AND.IWRK(IPE2+5).EQ.0).OR.(IWRK(IPE1+4).EQ.1.AND.IWRK(I
     +PE1+5).EQ.0.AND.IWRK(IPE2+4).EQ.0.AND.IWRK(IPE2+5).EQ.0))) GO TO 1
     +0191
C
C Process a left intersection.
C
            IF (.NOT.(IWRK(IPE1+9).EQ.0)) GO TO 10192
              IERR=4
              GO TO 10161
10192       CONTINUE
C
            IF (.NOT.(IG03.NE.0)) GO TO 10194
              IPSN=IG03
              IG03=IWRK(IG03)
            GO TO 10195
10194       CONTINUE
              IPWU=IPWU-3
              IF (.NOT.(IPWU.LE.IPWL)) GO TO 10196
                GO TO 10017
10196         CONTINUE
              IPSN=IPWU
10195       CONTINUE
C
            RWRK(IPSN  )=XINT
            RWRK(IPSN+1)=YINT
C
            IWRK(IPSN+2)=0
            IWRK(IWRK(IWRK(IPE1+9)+1)+2)=IPSN
            IWRK(IWRK(IPE1+9)+1)=IPSN
C
C Check for a right intersection.
C
          GO TO 10179
10191     CONTINUE
          IF (.NOT.((IWRK(IPE1+4).EQ.0.AND.IWRK(IPE1+5).EQ.1.AND.IWRK(IP
     +E2+4).EQ.1.AND.IWRK(IPE2+5).EQ.1).OR.(IWRK(IPE1+4).EQ.1.AND.IWRK(I
     +PE1+5).EQ.1.AND.IWRK(IPE2+4).EQ.0.AND.IWRK(IPE2+5).EQ.1))) GO TO 1
     +0198
C
C Process a right intersection.
C
            IF (.NOT.(IWRK(IPE2+9).EQ.0)) GO TO 10199
              IERR=5
              GO TO 10161
10199       CONTINUE
C
            IF (.NOT.(IG03.NE.0)) GO TO 10201
              IPSN=IG03
              IG03=IWRK(IG03)
            GO TO 10202
10201       CONTINUE
              IPWU=IPWU-3
              IF (.NOT.(IPWU.LE.IPWL)) GO TO 10203
                GO TO 10017
10203         CONTINUE
              IPSN=IPWU
10202       CONTINUE
C
            RWRK(IPSN  )=XINT
            RWRK(IPSN+1)=YINT
C
            IWRK(IPSN+2)=IWRK(IWRK(IPE2+9))
            IWRK(IWRK(IPE2+9))=IPSN
C
C Check for a local maximum.
C
          GO TO 10179
10198     CONTINUE
          IF (.NOT.((IWRK(IPE1+4).EQ.1.AND.IWRK(IPE1+5).EQ.1.AND.IWRK(IP
     +E2+4).EQ.0.AND.IWRK(IPE2+5).EQ.0).OR.(IWRK(IPE1+4).EQ.0.AND.IWRK(I
     +PE1+5).EQ.1.AND.IWRK(IPE2+4).EQ.1.AND.IWRK(IPE2+5).EQ.0))) GO TO 1
     +0205
C
C Process a local maximum.
C
            IF (.NOT.(IWRK(IPE1+9).EQ.0.OR.IWRK(IPE2+9).EQ.0)) GO TO 102
     +06
              IERR=6
              GO TO 10161
10206       CONTINUE
C
            IPP1=IWRK(IPE1+9)
            IPP2=IWRK(IPE2+9)
C
            IWRK(IPE1+9)=0
            IWRK(IPE2+9)=0
C
            IF (.NOT.(IG03.NE.0)) GO TO 10208
              IPSN=IG03
              IG03=IWRK(IG03)
            GO TO 10209
10208       CONTINUE
              IPWU=IPWU-3
              IF (.NOT.(IPWU.LE.IPWL)) GO TO 10210
                GO TO 10017
10210         CONTINUE
              IPSN=IPWU
10209       CONTINUE
C
            RWRK(IPSN  )=XINT
            RWRK(IPSN+1)=YINT
C
            IWRK(IPSN+2)=IWRK(IPP1)
            IWRK(IPP1)=IPSN
C
C See if the meeting edges are contributing to the same polygon.
C
            IF (.NOT.(IPP1.NE.IPP2)) GO TO 10212
C
C They aren't.  Append the subsidiary nodes of one polygon to the other.
C
              IWRK(IWRK(IPP2+1)+2)=IPSN
              IWRK(IPP2+1)=IWRK(IPP1+1)
C
C Remove from the polygon list the polygon whose subsidiary nodes have
C become part of the other polygon and put its principal node on the
C garbage list for 3-word nodes, so that it can be re-used.
C
              IF (.NOT.(IPPL.EQ.IPP1)) GO TO 10213
                IPPL=IWRK(IPP1+2)
              GO TO 10214
10213         CONTINUE
                ISPL=IPPL
10215           CONTINUE
                  IF (.NOT.(IWRK(ISPL+2).EQ.IPP1)) GO TO 10216
                    IWRK(ISPL+2)=IWRK(IPP1+2)
                    GO TO 10217
10216             CONTINUE
                  ISPL=IWRK(ISPL+2)
                GO TO 10215
10217           CONTINUE
10214         CONTINUE
C
              IWRK(IPP1)=IG03
              IG03=IPP1
C
C Any AET node that referenced IPP1 must now reference IPP2 instead.
C
              IDUM=IAET
C
10218         CONTINUE
              IF (.NOT.(IDUM.NE.0)) GO TO 10219
                IF (IWRK(IDUM+9).EQ.IPP1) IWRK(IDUM+9)=IPP2
                IDUM=IWRK(IDUM+6)
              GO TO 10218
10219         CONTINUE
C
10212       CONTINUE
C
10179     CONTINUE
10205     CONTINUE
C
C Swap the positions of edge 1 and edge 2 in the AET.
C
          IF (IWRK(IPE1+7).NE.0) IWRK(IWRK(IPE1+7)+6)=IPE2
          IF (IWRK(IPE2+6).NE.0) IWRK(IWRK(IPE2+6)+7)=IPE1
          IWRK(IPE1+6)=IWRK(IPE2+6)
          IWRK(IPE2+7)=IWRK(IPE1+7)
          IWRK(IPE1+7)=IPE2
          IWRK(IPE2+6)=IPE1
C
C If the AET started with edge 1, it now starts with edge 2.
C
          IF (IAET.EQ.IPE1) IAET=IPE2
C
C Exchange the polygon pointers of edges 1 and 2.
C
          IDUM=IWRK(IPE1+9)
          IWRK(IPE1+9)=IWRK(IPE2+9)
          IWRK(IPE2+9)=IDUM
C
C Advance to the next point of intersection in the list.
C
          IINT=IWRK(IINT+4)
C
C Quit if there are no more points of intersection to process.
C
          IF (IINT.EQ.0) GO TO 10220
C
C End of loop on points of intersection.
C
        GO TO 10156
10220   CONTINUE
C
C End of internal procedure to process a list of intersections.
C
      GO TO (10110,10141) , L10111
C
C The following internal procedure processes an edge in the AET that is
C terminating at the top of the current scanbeam.  The variable ITMP
C points to the edge that is to be processed.  If the edge is removed
C from the AET (which can happen), the procedure must adjust the value
C of ITMP so that the next-node pointer in the AET node that ITMP
C points at properly specifies the next AET node to be examined.
C
10113 CONTINUE
C
C Find the index, in the user's arrays, of the end point of the
C successor edge.
C
        INNP=ABS(IWRK(ITMP+8))+SIGN(1,IWRK(ITMP+8))
C
C Extract the X and Y coordinates of the end point of the successor
C edge.
C
        IF (.NOT.(IWRK(ITMP+4).EQ.0)) GO TO 10221
          IF (.NOT.(INNP.LT.1)) GO TO 10222
            INNP=INNP+LCCP
          GO TO 10223
10222     CONTINUE
          IF (.NOT.(INNP.GT.LCCP)) GO TO 10224
            INNP=INNP-LCCP
10223     CONTINUE
10224     CONTINUE
          XCNP=XCCP(INNP)
          YCNP=YCCP(INNP)
        GO TO 10225
10221   CONTINUE
          IF (.NOT.(INNP.LT.1)) GO TO 10226
            INNP=INNP+LCSP
          GO TO 10227
10226     CONTINUE
          IF (.NOT.(INNP.GT.LCSP)) GO TO 10228
            INNP=INNP-LCSP
10227     CONTINUE
10228     CONTINUE
          XCNP=XCSP(INNP)
          YCNP=YCSP(INNP)
10225   CONTINUE
C
C Check the vertical position of the end point of the successor edge.
C
        IF (.NOT.(YCNP.GE.YTOS)) GO TO 10229
C
C The end point of the successor edge is above the top of the scanbeam.
C
C Check whether the edge is contributing to a polygon.
C
          IF (.NOT.(IWRK(ITMP+9).NE.0)) GO TO 10230
C
C The edge is contributing to a polygon.  Form a subsidiary polygon
C node to add to that polygon.
C
            IF (.NOT.(IG03.NE.0)) GO TO 10231
              IPSN=IG03
              IG03=IWRK(IG03)
            GO TO 10232
10231       CONTINUE
              IPWU=IPWU-3
              IF (.NOT.(IPWU.LE.IPWL)) GO TO 10233
                GO TO 10017
10233         CONTINUE
              IPSN=IPWU
10232       CONTINUE
C
            RWRK(IPSN  )=RWRK(ITMP)
            RWRK(IPSN+1)=YTOS
C
C Add the end point of the current edge to either the left end or the
C right end of the polygon to which the edge is contributing, whichever
C is appropriate.
C
            IF (.NOT.(IWRK(ITMP+5).EQ.1)) GO TO 10235
              IWRK(IPSN+2)=IWRK(IWRK(ITMP+9))
              IWRK(IWRK(ITMP+9))=IPSN
            GO TO 10236
10235       CONTINUE
              IWRK(IPSN+2)=0
              IWRK(IWRK(IWRK(ITMP+9)+1)+2)=IPSN
              IWRK(IWRK(ITMP+9)+1)=IPSN
10236       CONTINUE
C
10230     CONTINUE
C
C Update the node to represent its successor edge.
C
          RWRK(ITMP+1)=XCNP
          RWRK(ITMP+2)=YCNP
C
          IF (.NOT.(YCNP.NE.YTOS)) GO TO 10237
            RWRK(ITMP+3)=(XCNP-RWRK(ITMP))/(YCNP-YTOS)
          GO TO 10238
10237     CONTINUE
            RWRK(ITMP+3)=SIGN(RBIG,XCNP-RWRK(ITMP))
10238     CONTINUE
C
          IWRK(ITMP+8)=SIGN(INNP,IWRK(ITMP+8))
C
        GO TO 10239
10229   CONTINUE
C
C The end point of the successor edge is below the top of the scanbeam.
C We have arrived at a local maximum, so handle that case.
C
          IF (.NOT.(IWRK(ITMP+6).EQ.0)) GO TO 10240
            IERR=7
            GO TO 10161
10240     CONTINUE
C
          IPP1=IWRK(ITMP+9)
          IPP2=IWRK(IWRK(ITMP+6)+9)
C
          IF (.NOT.(IPP1.NE.0.OR.IPP2.NE.0)) GO TO 10242
C
            IF (.NOT.(IPP1.EQ.0.OR.IPP2.EQ.0)) GO TO 10243
              IERR=8
              GO TO 10161
10243       CONTINUE
C
            IF (.NOT.(IG03.NE.0)) GO TO 10245
              IPSN=IG03
              IG03=IWRK(IG03)
            GO TO 10246
10245       CONTINUE
              IPWU=IPWU-3
              IF (.NOT.(IPWU.LE.IPWL)) GO TO 10247
                GO TO 10017
10247         CONTINUE
              IPSN=IPWU
10246       CONTINUE
C
            RWRK(IPSN  )=RWRK(ITMP)
            RWRK(IPSN+1)=YTOS
C
            IF (.NOT.(IWRK(ITMP+5).EQ.1)) GO TO 10249
              IWRK(IPSN+2)=IWRK(IPP1)
              IWRK(IPP1)=IPSN
            GO TO 10250
10249       CONTINUE
              IWRK(IPSN+2)=0
              IWRK(IWRK(IPP1+1)+2)=IPSN
              IWRK(IPP1+1)=IPSN
10250       CONTINUE
C
C See if the meeting edges are contributing to the same polygon.
C
            IF (.NOT.(IPP1.NE.IPP2)) GO TO 10251
C
C They aren't.  Append the subsidiary nodes of one polygon to the other.
C
              IF (.NOT.(IWRK(ITMP+5).EQ.1)) GO TO 10252
                IWRK(IWRK(IPP2+1)+2)=IWRK(IPP1)
                IWRK(IPP2+1)=IWRK(IPP1+1)
              GO TO 10253
10252         CONTINUE
                IWRK(IWRK(IPP1+1)+2)=IWRK(IPP2)
                IWRK(IPP2)=IWRK(IPP1)
10253         CONTINUE
C
C Remove from the polygon list the polygon whose subsidiary nodes have
C become part of the other polygon and put its principal node on the
C garbage list for 3-word nodes, so that it can be re-used.
C
              IF (.NOT.(IPPL.EQ.IPP1)) GO TO 10254
                IPPL=IWRK(IPP1+2)
              GO TO 10255
10254         CONTINUE
                ISPL=IPPL
10256           CONTINUE
                  IF (.NOT.(IWRK(ISPL+2).EQ.IPP1)) GO TO 10257
                    IWRK(ISPL+2)=IWRK(IPP1+2)
                    GO TO 10258
10257             CONTINUE
                  ISPL=IWRK(ISPL+2)
                GO TO 10256
10258           CONTINUE
10255         CONTINUE
C
              IWRK(IPP1)=IG03
              IG03=IPP1
C
C Any AET node that referenced IPP1 must now reference IPP2 instead.
C
              IDUM=IAET
C
10259         CONTINUE
              IF (.NOT.(IDUM.NE.0)) GO TO 10260
                IF (IWRK(IDUM+9).EQ.IPP1) IWRK(IDUM+9)=IPP2
                IDUM=IWRK(IDUM+6)
              GO TO 10259
10260         CONTINUE
C
10251       CONTINUE
C
10242     CONTINUE
C
C Delete from the AET the edge ITMP and the edge that follows it.  The
C nodes go back on the garbage list for 10-word nodes.
C
          ITM1=IWRK(ITMP+7)
          ITM2=IWRK(IWRK(ITMP+6)+6)
C
          IF (.NOT.(ITM1.EQ.0)) GO TO 10261
            IAET=ITM2
          GO TO 10262
10261     CONTINUE
            IWRK(ITM1+6)=ITM2
10262     CONTINUE
C
          IF (ITM2.NE.0) IWRK(ITM2+7)=ITM1
C
          IWRK(ITMP)=IWRK(ITMP+6)
          IWRK(IWRK(ITMP))=IG10
          IG10=ITMP
C
C Adjust the pointer into the AET so as to continue looping properly.
C
          ITMP=IWRK(ITMP+6)
C
10239   CONTINUE
C
      GO TO (10112,10145) , L10113
C
C Error exits.
C
10003 CONTINUE
        IERR=1
        RETURN
C
10006 CONTINUE
        IERR=2
        RETURN
C
10017 CONTINUE
        IERR=3
        RETURN
C
10161 CONTINUE
        IERR=3+IERR
        RETURN
C
      END
