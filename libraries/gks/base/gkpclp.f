C# IL>=a, OL>=0
      SUBROUTINE GKPCLP(IPOLY,ISZ,IPLIST,VINX,VINY,
     :                     XMIN,XMAX,YMIN,YMAX,VOUTX,VOUTY)
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
*     Clips Vertices of input polygon(s) successively against the four
*     edges of an arbitrary Clipping rectangle, yielding a new list of
*     polygons. Clip-induced edge lines are included, and an arbitrary
*     number of polygons may result.
*
*  MAINTENANCE LOG
*  ---------------
*     08/03/83  NGB  Original version stabilized
*     06/06/83  AS   Remove diagnostics, change variables to conform with
*                    standards.
*     04/07/83  PGLS Change KERROR
*     13/01/84  AS   Prevent ISZ being used as a local variable
*     02/03/84  MGC   Replace -1 with KNIL
*
*  ARGUMENTS
*  ---------
*     INP IPOLY  no of Polygons (normally only one supplied):
*     INP ISZ    dimensioner for arrays:
*     INP IPLIST array for last vertex indices for each polygon:
*     INP VINX,VINY Input Vertex coords:
*     INP XMIN,XMAX,YMIN,YMAX Clipping Rectangle
*     OUT VOUTX,VOUTY arrays for Output Vertex coords:
*
      INTEGER IPOLY, ISZ, IPLIST(ISZ/2)
      REAL VINX(ISZ), VINY(ISZ), XMIN, XMAX, YMIN, YMAX,
     :     VOUTX(ISZ*2),VOUTY(ISZ*2)
*
* Note:
*   ISZ is the supplied size of VINX/Y, (ie the no of input vertices)
*   VOUTX/Y must be able to hold at least IPLIST(IPOLY)*2 elements
*   IPLIST must be able to hold at least IPLIST(IPOLY)/2 elements
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
      INTEGER ISIZ, IXBASE, IYBASE, ILBASE, IEDGE, I, IVSIZ
*
*  STACK USAGE
*  -----------
*     We need workspace to buffer partial results between clip stages.
*     We can generate a maximum of 2*N vertices, and N/2 polygons.
*     Since polygons are enclosed by dummy 'barrier' entries on the
*     Link array, we need 2.5*N + 1 (+ a half for rounding)
*     i.e  ISIZ=IFIX(IPLIST(IPOLY)*2.5+1.5)
*     Three arrays of this size are required: two REAL arrays for
*     coordinates, and an INTEGER array to hold the Link information.
*
*  ALGORITHM
*  ---------
*      GKCLIP is invoked for each of the four Clip Edges successively
*      intermediate results being stored in VOUTX,VOUTY.
*      After each stage of clipping
*          IPOLY      -is the number of discrete polygons
*          IPLIST(n)  -is the index into VOUTX,VOUTY of the last vertex
*                      belonging to polygon <n>
*      so IPLIST(IPOLY) is the highest vertex index used in VOUTX/Y
*
*  COMMENTS
*  --------
*      - stack exhaustion reported via returned IRESP
*      - There is no reason why this algorithm should not be invoked
*      with more than one input polygon (say, to clip a polygon with a
*      hole in it).Any changes in topology will be correctly dealt with,
*      so for example, an outline 'O', specified by two input polygons,
*      might get clipped to a 'C' yielding one output polygon.
*
*---------------------------------------------------------------------


      IVSIZ = ISZ
      IF (IPOLY .EQ. 0) GOTO 1000
* there are one or more polygons..
      IF (IPLIST(IPOLY).LE.2) GOTO 1000
      DO 900  I = 1,IPLIST(IPOLY)
* ..that have three or more vertices between them

* transfer them initially to VOUT
         VOUTX(I) = VINX(I)
         VOUTY(I) = VINY(I)
  900 CONTINUE


* the following gets executed before calling clip:
* we can generate a maximum of 2*N vertices, and N/2 polygons.
*   Since polygons are enclosed by dummy 'barrier' entries in
* the workspace, we need 2.5*N + 1 (+ a half for rounding)
      ISIZ=IFIX(IPLIST(IPOLY)*2.5+1.5)

* pre-set stack base pointers for tidy collapse
      ILBASE=KNIL
      IXBASE=KNIL
      IYBASE=KNIL
* acquire sufficient workspace
      CALL GKSTAL(KINTGS,ISIZ,ILBASE)
      IF (KERROR.NE.0) GOTO 999
      CALL GKSTAL(KREALS,ISIZ,IXBASE)
      IF (KERROR.NE.0) GOTO 999
      CALL GKSTAL(KREALS,ISIZ,IYBASE)
      IF (KERROR.NE.0) GOTO 999
* After each stage of clipping
*    IPOLY      -is the number of discrete polygons
*    IPLIST(n)  -is the index into VOUTX,VOUTY of the last vertex
*               belonging to polygon <n>
* so IPLIST(IPOLY) is the highest vertex index used in VOUTX/Y

* clip top
      IEDGE=1
      CALL GKCLIP(IPOLY,IVSIZ,IPLIST,VOUTX,VOUTY,IEDGE,XMIN,XMAX,
     :               YMIN,YMAX,ISIZ,KSTACK(ILBASE),QSTACK(IXBASE),
     :               QSTACK(IYBASE))

* clip right
      IF (IPOLY .EQ. 0) GOTO 999
      IF (IPLIST(IPOLY) .EQ. 0) GOTO 999
      IVSIZ=IPLIST(IPOLY)
      IEDGE=2
      CALL GKCLIP(IPOLY,IVSIZ,IPLIST,VOUTX,VOUTY,IEDGE,XMIN,XMAX,
     :               YMIN,YMAX,ISIZ,KSTACK(ILBASE),QSTACK(IXBASE),
     :               QSTACK(IYBASE))
* clip bottom
      IF (IPOLY .EQ. 0) GOTO 999
      IF (IPLIST(IPOLY) .EQ. 0) GOTO 999
      IVSIZ=IPLIST(IPOLY)
      IEDGE=3
      CALL GKCLIP(IPOLY,IVSIZ,IPLIST,VOUTX,VOUTY,IEDGE,XMIN,XMAX,
     :               YMIN,YMAX,ISIZ,KSTACK(ILBASE),QSTACK(IXBASE),
     :               QSTACK(IYBASE))
* clip left
      IF (IPOLY .EQ. 0) GOTO 999
      IF (IPLIST(IPOLY) .EQ. 0) GOTO 999
      IVSIZ=IPLIST(IPOLY)
      IEDGE=4
      CALL GKCLIP(IPOLY,IVSIZ,IPLIST,VOUTX,VOUTY,IEDGE,XMIN,XMAX,
     :               YMIN,YMAX,ISIZ,KSTACK(ILBASE),QSTACK(IXBASE),
     :               QSTACK(IYBASE))

* deallocate workspace (in reverse order)
  999 CONTINUE
      CALL GKSTDA(KREALS,IYBASE)
      CALL GKSTDA(KREALS,IXBASE)
      CALL GKSTDA(KINTGS,ILBASE)

 1000 CONTINUE

      END
