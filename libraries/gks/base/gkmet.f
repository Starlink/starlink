C# IL>=a, OL>=0
      SUBROUTINE GKMET(ICOUNT,VERTX,VERTY,RETLOY,RETHIY,IETMAX,
     :                    RETX,RETY,RETDX,RETEY,IETNXT)
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
*     Constructs a sorted Edge Table from the supplied polygon
*
*  MAINTENANCE LOG
*  ---------------
*      7/03/83  NGB   Original version stabilized
*      3/06/83  AS    Put in CONTINUEs, change integers to begin with
*                     right letters, remove diagnostics.
*     26/01/84  MGC   Remove redundant code
*     20/08/85  RMK   Split IF statements which tested whether a variable was
*                     non-zero and also used it as an array index (S125).
*
*  ARGUMENTS
*  ---------
*     INP ICOUNT  No of vertices
*     INP VERTX,VERTY  Vertex coordinates
*     INP RETLOY  Edge table lowest scanline
*     INP RETHIY  Edge table highest scanline
*     INP IETMAX  Edge table maximum entry used
*     INP RETX    Edge table X-start array
*     INP RETY    Edge table Y-start array
*     INP RETDX   Edge table slope array
*     INP RETEY   Edge table Y-limit array
*     INP IETNXT  Edge table link array for chaining waiting and active
*                 lists
*
      INTEGER ICOUNT, IETMAX, IETNXT(0:ICOUNT)
      REAL VERTX(ICOUNT), VERTY(ICOUNT), RETLOY, RETHIY, RETX(0:ICOUNT),
     :     RETY(0:ICOUNT), RETDX(0:ICOUNT), RETEY(0:ICOUNT)
*
*     Note:  Though on stack, the Edge Table array bases are passed
*     down as parameters so that they may be re-indexed from zero
*
*  LOCALS
*  ------
      INTEGER ITOP,IV,ILAST,I,J
      REAL THISX,THISY,XLAST,YLAST,AX,AY,DX,DY,EY
*
*  ALGORITHM
*  ---------
*     Constructs a sorted Edge Table from the supplied polygon.
*
*     ICOUNT contains the length of the vertex arrays, (or at least,
*     the length we are interested in).
*
*     It also contains the extent of the stack-space arrays acquired
*     for use as the components of the Edge Table: -the actual arguments
*     corresponding to RETX,RETY,RETDX,RETEY and IETNXT,
*     will be the first element assigned to each array (indexed as 0).
*
*     on Exit:IETMAX will hold the used size of the Edge Table
*             RETLOY..RETHIY will indicate the range of scanlines needed
*
*---------------------------------------------------------------------


      ITOP = 0
      IETNXT(0) = 0
* start with empty ET
      RETLOY = VERTY(1)
      RETHIY = VERTY(1)

* we will be starting with the edge from vertex ICOUNT to vertex 1
      ILAST=ICOUNT
      THISX = VERTX(ILAST)
      THISY = VERTY(ILAST)
      IV = 1

   30 CONTINUE
*  repeat until IV=ILAST...
         XLAST = THISX
         YLAST = THISY
         THISX = VERTX(IV)
         THISY = VERTY(IV)

*  update extent
         IF (THISY .GT. RETHIY) RETHIY = THISY
         IF (THISY .LT. RETLOY) RETLOY = THISY
         IF (THISY .NE. YLAST) THEN
*  (otherwise skip horizontal line altogether)

*  find end with max Y
            IF (THISY .GT. YLAST) THEN
               AX = THISX
               AY = THISY
               DX = XLAST-THISX
               DY = YLAST-THISY
               EY = YLAST
            ELSE
*  YLAST > ThisY
               AX = XLAST
               AY = YLAST
               DX = THISX-XLAST
               DY = THISY-YLAST
               EY = THISY
            ENDIF


*  now find a home for it
            I = 0
            J = IETNXT(I)

*  sort on higher Y
   40       CONTINUE
            IF (J.NE.0) THEN
               IF (RETY(J) .GT. AY) THEN
                  I = J
                  J = IETNXT(I)
                  GOTO 40
               ENDIF
            ENDIF

*  sort on lower X
   50       CONTINUE
            IF (J.NE.0) THEN
               IF ((RETY(J).EQ.AY).AND.(RETX(J).LT.AX)) THEN
                  I = J
                  J = IETNXT(I)
                  GOTO 50
               ENDIF
            ENDIF

*  sort on lower DX
   60       CONTINUE
            IF (J.NE.0) THEN
               IF ((RETY(J).EQ.AY) .AND. (RETX(J).EQ.AX)
     :             .AND. (RETDX(J).LT.DX)) THEN
                  I = J
                  J = IETNXT(I)
                  GOTO 60
               ENDIF
            ENDIF

*  we need to insert new edge between (I) and (J)

*  allocate new ET slot
            ITOP = ITOP+1
            RETX(ITOP) = AX
            RETY(ITOP) = AY
            RETDX(ITOP) = -DX/DY
*  DY is always negative/zero
            RETEY(ITOP) = EY
            IETNXT(ITOP) = J
            IETNXT(I) = ITOP
         ENDIF
         IV = IV+1
* repeating until IV = ILAST for this polygon
      IF (IV .LE. ILAST) GOTO 30

* return used size of Edge Table
      IETMAX = ITOP

      END
