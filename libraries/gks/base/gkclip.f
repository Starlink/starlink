C# IL>=a, OL>=0
      SUBROUTINE GKCLIP(IPOLY,IVSIZ,IPLIST,VERTX,VERTY,ICLIP,XMIN,
     :                     XMAX,YMIN,YMAX,ISIZ,LINK,CURRX,CURRY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             NGB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To clip the edges of an input set of polygons against a Clip Edge,
*     yielding a new set of vertices belonging to a new set of polygons.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/83  NGB  Original version stabilized
*     19/04/83  PGLS Change name to GKCLIP
*     20/08/85  RMK  Removed unused local variables NEXT and LLEN.
*
*  ARGUMENTS
*  ---------
*     INP IPOLY  no of polygons in use
*     INP IVSIZ  original no of vertices (gives dimensions of arrays)
*     INP IPLIST array of last vertex indices for each polygon
*     INP VERTX,VERTY  array of Input Vertex coords
*     INP ICLIP  Edge to clip on
*     INP XMIN,XMAX,YMIN,YMAX  Clip Spec
*     INP ISIX   Workspace dimensions
*     INP LINK   Link array
*     INP CURRX,CURRY  Current Vertex coords
*
      INTEGER IPOLY, IVSIZ, IPLIST(IVSIZ/2), ICLIP, ISIZ, LINK(0:ISIZ-1)
      REAL VERTX(IVSIZ*2), VERTY(IVSIZ*2), XMIN, XMAX, YMIN, YMAX,
     :     CURRX(0:ISIZ-1), CURRY(0:ISIZ-1)
*
*  LOCALS
*  ------
      INTEGER IFIRST, ITHIS, LAST, I, IC, ISTART, IP, IPREV, IDIR,
     :         IV, LI, IPOLYC, ILASTC, ICPOLY, LASTV
      LOGICAL DONE, CLOSE, THSOUT, PRVOUT, DIDLNK, FOUND, GKQOUT
      REAL X, Y
*
*  ALGORITHM
*  ---------
*     There are three stages to the processing:
*       STAGE1:- Clips supplied points in VERTX/Y against Clipping Edge;
*       STAGE2:- Links intersections, in pairs, along the Clip Edge;
*       STAGE3:- Follows interior and edge spans,
*                producing a new list of points in VERTX/Y,
*                detecting polygons as completed,
*                and setting up IPLIST to describe them;
*
*      Firstly, sufficient workspace is obtained to hold the maximum
*      number of intermediate vertices (twice the supplied number).
*      Space is also obtained to hold an array of Links, one per vertex,
*      which will be used to control STAGE3.
*
*     1) Each successive vertex within each source polygon is checked
*        against the clip edge giving the four possibilities:
*        a) it is inside, and so was the last, so put it in the current
*           list
*        b) it is outside, and so was the last, so ignore it
*        c) it is outside, but the last was inside, so save the
*           intersection
*        d) it is inside, but the last was outside, so save the
*           intersection, together with the new vertex
*
*     2) All vertices are sorted along the clipping edge, and successive
*        pairs of edge vertices made to point at each other
*
*     3) Points are transferred to the output list under the control of
*        the links, which are cancelled as processed. When a ring of
*        links has been traversed, a polygon is complete: the count of
*        vertices is put into IPLIST,  and the link list searched for
*        unprocessed links.  -If any are found they belong to a further
*        polygon...
*
*     When all vertices have been transferred to the output list the
*     subroutine returns, and clipping against the next edge will be
*     invoked.
*
*
*     The LINK integer array holds the type of each item being held in
*     the Current Vertex array:
*
*     Its entries may take the values:
*         0: a non-edge vertex
*      1..n: index of 'partner' of this edge vertex
*     -9999: vertex already dealt with
*     -8888: a 'barrier' delimiting a loop of vertices
*            (the CURRX/Y entries for a 'barrier' Link index are unused)
*
*---------------------------------------------------------------------


********************
*     STAGE 1      *
********************

* clip the vertices of all input polygons from VERTX/Y into CURRX/Y


      ITHIS=1
      IP = 1
      IC = 1
      LINK(IC) = -8888
* erect first barrier(initialise CURRX/Y so we can dump them)
      CURRX(IC) = -8888.0
      CURRY(IC) = -8888.0
      LINK(0) = 0
      DO 10  IP=1,IPOLY
* for each polygon...

         ISTART = IC

* continue where we left off
         IFIRST = ITHIS

* ITHIS will stop one beyond last vertex
         LAST = IPLIST(IP)
         IPREV = LAST
         PRVOUT = GKQOUT(VERTX(IPREV),VERTY(IPREV),
     :                      ICLIP,XMIN,XMAX,YMIN,YMAX)
* as long as this polygon has three or more vertices...
         IF (LAST.GT.(IFIRST+1)) THEN
   20       CONTINUE
*
* if ITHIS vertex is the opposite side of ICLIP from the PREVious one
* then the intersection is computed and sorted into the linked list
* of intersections on increasing X/Y
*
* if ITHIS vertex is interior then it is included

               THSOUT = GKQOUT(VERTX(ITHIS),VERTY(ITHIS),
     :                           ICLIP,XMIN,XMAX,YMIN,YMAX)
* leaving or entering ?
               IF ((THSOUT.AND..NOT.PRVOUT).OR.(.NOT.THSOUT.AND.PRVOUT))
     :         THEN
***** put intersection into CURRX/Y****
                  IC = IC+1
                  CALL GKSECT(VERTX(IPREV),VERTY(IPREV),VERTX(ITHIS),
     :                       VERTY(ITHIS),ICLIP,XMIN,XMAX,YMIN,YMAX,X,Y)
                  CURRX(IC) = X
                  CURRY(IC) = Y
                  I = 0
                  FOUND =  .FALSE.
* find where to insert edge in list
   30             CONTINUE
                  IF ((.NOT.FOUND).AND.(LINK(I).NE.0)) THEN
* determine if IC is beyond LINK(I), along ICLIP
                     IF ( ( ((ICLIP.EQ.1).OR.(ICLIP.EQ.3))
     :                        .AND.(CURRX(IC) .GT. CURRX(LINK(I))) )
     :                   .OR.( ((ICLIP.EQ.2).OR.(ICLIP.EQ.4))
     :                       .AND.(CURRY(IC) .GT. CURRY(LINK(I))) ))THEN
* (it is beyond I)
                        I = LINK(I)
* so try next one
                     ELSE
                        FOUND = .TRUE.
                     ENDIF
                     GOTO 30
                  ENDIF
*drop through if LINK(I) is zero

* we need to insert new edge vertex after I
                  LINK(IC) = LINK(I)
                  LINK(I) = IC
               ENDIF

****(finished processing intersect)****



* now see whether vertex to be included

               IF(.NOT.THSOUT) THEN
* (yes)
                  IC = IC+1
                  CURRX(IC) = VERTX(ITHIS)
                  CURRY(IC) = VERTY(ITHIS)
* interior vertices must have link set to zero
                  LINK(IC) = 0
               ENDIF

               IPREV = ITHIS
               PRVOUT = THSOUT
               ITHIS = ITHIS+1
            IF (.NOT. ITHIS.GT.LAST) GOTO 20
*           ...while...loop
         ENDIF
*        ...non-degenerate
* (ITHIS ends on one beyond last vertex)




***** clipping complete for that input polygon..
         IF (IC .GT. ISTART) THEN
* ..so as long as there's something left of it..
            IC = IC+1
            LINK(IC) = -8888
* ..erect a barrier between it and the next polygon
* (initialise CURRX/Y so we can dump them)
            CURRX(IC) = -8888
            CURRY(IC) = -8888
         ENDIF
* now continue with any further polygon(s)
   10 CONTINUE

***** all polygons now clipped
      ICPOLY = IC
* remember how many went into Current vertex list


********************
*     STAGE 2      *
********************


* pair off edge vertices, to make 'partners' point to each other

      IFIRST = LINK(0)
*
* as long as there is at least one intersection,
*                     there will be at least two..

   40 CONTINUE
      IF (IFIRST .NE. 0) THEN
         LAST = LINK(IFIRST)
* <ITHIS> will be first of next pair
         ITHIS = LINK(LAST)
         LINK(LAST) = IFIRST
         IFIRST = ITHIS
         GOTO 40
      ENDIF
***** all intersections paired

********************
*     STAGE 3      *
********************



* we are now ready to process the vertex list CURRX/Y,
* delivering closed polygons into IPLIST & VERTX/Y

* ICPOLY is the count of entries in CURRX/Y

* IPOLYC is the count of output vertices put into VERTX/Y
      IPOLYC = 0
* IPOLY is the count of output polygons detected.
*  (for each polygon completed, IPLIST(IPOLY) will show
*   the highest vertex belonging to that polygon)
      IPOLY = 0
* initially...
      CLOSE =  .TRUE.
      ILASTC = 0


   50 CONTINUE
* the processing of vertex CURRX/Y(IV) under the control of LINK(IV)

         IF (CLOSE) THEN
* we just finished a polygon
* (..except first time through of course)
            CLOSE =  .FALSE.
            DIDLNK =  .FALSE.

            IF (IPOLYC.GT.ILASTC) THEN
* the one we finished didn't degenerate to nothing
               IPOLY = IPOLY+1
               IPLIST(IPOLY) = IPOLYC
               ILASTC = IPOLYC
            ENDIF
*we need to find another polygon
            DONE =  .TRUE.
* ..until disproved
            IV = 1
   60       CONTINUE
            IF (DONE .AND. (IV .LE. ICPOLY)) THEN
* LINK(IV) will be negative for a barrier or a 'crossed off' vertex
               IF (LINK(IV) .GE. 0) DONE = .FALSE.
               IV = IV+1
               GOTO 60
            ENDIF

            IV = IV-1
            IDIR = +1

         ENDIF
*         ..(CLOSE)


         IF(.NOT.DONE) THEN
* process vertex Current(IV) under control of LINK(IV)
            LI = LINK(IV)
            IF (LI .EQ. 0) THEN


***** its an internal vertex...
* add Current vertex V onto Output list
               IPOLYC = IPOLYC+1
               VERTX(IPOLYC) = CURRX(IV)
               VERTY(IPOLYC) = CURRY(IV)
* cross it off
               LINK(IV) = -9999

* proceed in current direction,
* rolling round last to first of polygon
               CALL GKNEXT(IV,IDIR,ISIZ,LINK)
               IF (IV .EQ. 0) CLOSE =  .TRUE.
* ..hit one already processed, so polygon complete
               DIDLNK =  .FALSE.
            ELSE
*****  (LI > 0 for an edge vertex...)
* add Current vertex IV onto Output list
               IPOLYC = IPOLYC+1
               VERTX(IPOLYC) = CURRX(IV)
               VERTY(IPOLYC) = CURRY(IV)
* cross it off
               LINK(IV) = -9999

* find its partner unless we just came from there
               IF (DIDLNK) THEN
                  CALL GKNEXT(IV,IDIR,ISIZ,LINK)
                  IF (IV .EQ. 0) CLOSE =  .TRUE.
* ..hit one already processed, so polygon complete
                  DIDLNK =  .FALSE.
* 'cos we didn't link
               ELSE IF (LINK(LI) .EQ. -9999) THEN
* circuit completed
                  CLOSE =  .TRUE.
               ELSE
* (there are more loops in this polygon)
                  DIDLNK =  .TRUE.
* -to prevent oscillation
                  IV = LI
***** carry on from partner

* add Current vertex IV onto Output list
                  IPOLYC = IPOLYC+1
                  VERTX(IPOLYC) = CURRX(IV)
                  VERTY(IPOLYC) = CURRY(IV)
* cross it off
                  LINK(IV) = -9999

* The new processing direction depends on whether the new IV is at the
*      bottom or top of its block of internal vertices.
* There has got to be at least one adjacent internal vertex, so look
*      around for it and choose direction accordingly.


* (remember IV)
                  LASTV = IV
* try forwards first
                  IDIR = +1
                  CALL GKNEXT(IV,IDIR,ISIZ,LINK)
* if next one already processed
* or is not an interior vertex..
                  IF ((IV.EQ.0).OR.(LINK(IV).NE.0)) THEN
* ..then try backwards

* (restore  IV)
                     IV = LASTV

                     IDIR = -1
                     CALL GKNEXT(IV,IDIR,ISIZ,LINK)
* if that one already processed
* or is not an interior vertex..
                     IF ((IV .EQ. 0).OR.(LINK(IV).NE.0)) CLOSE = .TRUE.
* ..then there are no more in either direction
                  ENDIF

* IV is now the new interior vertex
               ENDIF
*              ..else link to partner...
            ENDIF
*           ...not interior, thus edge vertex..
         ENDIF
*        ...weren't DONE..
      IF (.NOT.DONE) GOTO 50
*     ..to continue processing vertices from this, or another, polygon

      END




      LOGICAL FUNCTION GKQOUT(VX,VY,ICLIP,XMIN,XMAX,YMIN,YMAX)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             NGB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     determines the relationship of vertex V to the clipping edge
*     specified by ICLIP:
*
*  MAINTENANCE LOG
*  ---------------
*     08/03/83  NGB  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP VX,VY   coords of vertex V
*     INP ICLIP   Edge to clip on
*     INP XMIN,XMAX,YMIN,YMAX Clip Spec
*
      INTEGER ICLIP
      REAL VX, VY, XMIN, XMAX, YMIN, YMAX
*
*  FUNCTION RETURN VALUE
*  ---------------------
*     returns .TRUE.  if  OUT
*             .FALSE. if  NOT OUT
*
*---------------------------------------------------------------------

      GKQOUT =.FALSE.
      GOTO (10,20,30,40), ICLIP
* top
   10 CONTINUE
      IF (VY .GT. YMAX) GKQOUT =.TRUE.
      GOTO 50

* right
   20 CONTINUE
      IF (VX .GT. XMAX) GKQOUT =.TRUE.
      GOTO 50

* bottom
   30 IF (VY .LT. YMIN) GKQOUT =.TRUE.
      GOTO 50

* left
   40 IF (VX .LT. XMIN) GKQOUT =.TRUE.

   50 CONTINUE
* Not Out!
      END
