C# IL>=a, OL>=0
      SUBROUTINE GKCRCS(IOPT,NRD,RX,RY,IFILSC,
     :                  LSIMUL,SIMLEN,LINSUB,ROSUB)
*
* (C) COPYRIGHT ICL & SERC  1988
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  UTILITY
*  Author:             JGW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To generate an arc/circle for GDP.
*
*  MAINTENANCE LOG
*  ---------------
*     14/11/83  JGW   Original version stabilized
*     20/12/83  AS    Tidy up
*     11/01/84  AS    Take out include for GDP.PAR
*     01/02/84  JGW   Added check for B not between AC
*     10/02/84  JGW   FI set THETA/NPTS (was npts+1).Also tightened
*                     validation & set end point to {CX,CY}
*     24/02/84  JGW   Renamed parameter npts as NRD (I144/S18)
*     02/04/84  JGW   Relaxed tolerance on  colinear test(I198)
*     02/04/84  JGW   Colinear points to give line if pie/chd/circ
*     10/05/84  JGW   Back transform code fixed
*                       -cure corruption of data in (circle case).
*                       -code moved, now assigns AX etc properly.
*     03/02/86  DRJF  PATTERN, HATCH and SOLID FILL AREA styles use
*                     SCAN LINES for there creation. This is
*                     inappropiate for the BENSON and other devices,
*                     hence the introduction of a scale factor which
*                     will ensure a more suitable spacing of the lines
*                     in HATCH and PATTERN styles. Note that GKCIRC
*                     has become GKFILS
*     19/01/87  RMK   IS conversion. Changed commenting of argument IOPT
*                     to use enumerated types.
*     05/06/87  RMK   Removed usage of GARC, GCHORD, GPIE, GCIRCL.
*     08/02/88  KEVP  Convert to make more efficient use of polylines,
*                     remove bug concerning number of lines in curve (S336)
*                     and tidy up structure, providing new utilities.
*     15/03/88  KEVP  Add argument DCIRC to GKCRCN. Not used here.
*     08/04/88  KEVP  Removed RADIUS argument from GKCRCV and
*                     added comments about the registration points.
*
*  ARGUMENTS
*  ---------
*     INP   IOPT    Curve option
*                     -1: arc unstyled
*                     -2: arc (chord) with interior styled
*                     -3: arc (pie) with interior styled
*                     -4: circle with interior styled
*     INP   RX }    Co-ordinates of three points on arc.
*     INP   RY }
*     INP   IFILSC  FILL AREA scale factor
*     INP   LSIMUL
*     INP   SIMLEN
*     INP   LINSUB  Line drawing function.
*     INP   ROSUB   Raster-op function.
*
      INTEGER IOPT,NRD, IFILSC
      REAL     RX(3), RY(3), SIMLEN
      LOGICAL  LSIMUL
      EXTERNAL LINSUB,ROSUB
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*   Values of IOPT
*     JARC   = -1 Plain Arc
*     JCHORD = -2 Chord  (fillable)
*     JPIE   = -3 Pie    (fillable)
*     JCIRC  = -4 Circle (fillable)
*
*   Values of ISTAT
*     JENDS  = 0  Arc with end points
*     JFULL  = 1  Full Circle
*     JLIN   = 2  Line
*
      INTEGER    JARC,    JCHORD,    JPIE,    JCIRC
      PARAMETER (JARC=-1, JCHORD=-2, JPIE=-3, JCIRC=-4)

      INTEGER    JENDS,   JFULL,   JLIN
      PARAMETER (JENDS=0, JFULL=1, JLIN=2)

*  Changing WC
*     AXFM       normalisation transform
*     BXFM       inverse AXFM (backtransform to WC)
*     TQCOPY     copy of w/s total transform (QWTOTT)
*
*  Arc variables
*     DXA,DYA    vector from First point of arc to Centre
*     DCIRC      True, if circular in DC (not used here)
*     IOFFST     Stack pointer of complete arc
*     IOFFSX                   of curve's X-coords
*     IOFFSY                   of curve's Y-coords
*     ISTAT      status of arc 0=with ends, 1=full, 2=line
*     NMAX       maximum number of points in curve
*     NPTS       number of points in arc curve
*     NPTSA      number of points in arc curve plus any extra
*     RADIUS     radius in WC
*     THETA      arc's turning angle (negative if clockwise)
*     XCEN,YCEN  centre of arc in WC
*     XPTS,YPTS  copy holding corrected points (back tranformed)
*
*  Arc Parameter
*     TWOPI      Two pi
*
      REAL TWOPI
      PARAMETER (TWOPI = 6.283185072)
      REAL DXA,DYA
      REAL XCEN,YCEN,RADIUS,THETA
      INTEGER I,ISTAT,IOFFST,IOFFSX,IOFFSY,NPTS,NPTSA
      LOGICAL DCIRC
      REAL    AXFM(3,2),BXFM(3,2),TQCOPY(6)
      REAL     XPTS(3), YPTS(3)
*
      INTEGER    NMAX
      PARAMETER (NMAX=120)

*  ALGORITHM
*  ---------
*     1: Obtain details of transform. Change world coords to
*        take account of the registration points.
*
*     2: Calculate the centre and radius in WC
*
*     3: Calculate the number of lines needed
*         -assume number of lines = Square root maximum radius (DC)
*          gives tidy arc.
*
*     4: Generate polygon in stack
*
*     5: Draw outline & fill/pattern inside as appropriate
*
*  COMMENTS
*  --------
*     The arc is circular in any coordinate system for which the
*     registration points form an isosceles (45 degree) right-angle
*     triangle with the third point at the right angle.
*     The WC are converted to such a coordinate system in this
*     utility.
*     When called from the GDP routine GGDP, the registation points
*     are (1,0), (0,1) & (1,1).
*---------------------------------------------------------------------

*     Check that number of points is correct for Curve Option.
      IF (.NOT.((NRD.EQ.3 .AND. IOPT.NE.JCIRC) .OR.
     :          (NRD.EQ.2 .AND. IOPT.EQ.JCIRC))
     :   )THEN
         KERROR = 100
         GOTO 9999
      ENDIF
*
*     Save original total segment transformation (ie WC to DC)
      DO 10 I=1,6
         TQCOPY(I) = QWTOTT(I,KWKIX)
   10 CONTINUE

*     Change world coords to take account of registration points.
*     (The original WC are restored at the end of the routine.)
      CALL GKMTDV(QWRA(1),QWRA(4),AXFM)
      CALL GKMTIV(AXFM,BXFM)
      CALL GKMTXF(BXFM,NRD,RX,RY,XPTS,YPTS)
      CALL GKMTML(AXFM,QWTOTT(1,KWKIX),QWTOTT(1,KWKIX))


*     -- handle circle parameters --
      IF (IOPT.EQ.JCIRC) THEN
         XPTS(3) = XPTS(1)
         YPTS(3) = YPTS(1)
         DXA    = XPTS(2) - XPTS(1)
         DYA    = YPTS(2) - YPTS(1)
         XCEN   = XPTS(2)
         YCEN   = YPTS(2)
         RADIUS = SQRT(DXA*DXA + DYA*DYA)
         THETA  = TWOPI
         ISTAT  = JFULL
      ELSE
         CALL GKCRCE (XPTS,YPTS,XCEN,YCEN,RADIUS,THETA,ISTAT)
      ENDIF
*
*     Calculate the number of points required to define arc
      IF(ISTAT .EQ. JLIN)THEN
         NPTS = 2
      ELSE
         CALL GKCRCN (RADIUS,THETA,NMAX,NPTS,DCIRC)
      ENDIF
*     Add extra pt for pie if required
      IF((ISTAT .EQ. JENDS) .AND. (IOPT .EQ. JPIE))THEN
        NPTSA = NPTS + 1
      ELSE
        NPTSA = NPTS
      ENDIF
*
*     Allocate Stack for these points
      CALL GKSTAL (KREALS,2*NPTSA,IOFFST)
      IF(KERROR .NE. 0)GOTO 9999
      IOFFSX = IOFFST
      IOFFSY = IOFFSX + NPTSA

*     Put centre into input array
      XPTS(2) = XCEN
      YPTS(2) = YCEN
*     Define the points on the curve
      CALL GKCRCV(XPTS,YPTS,THETA,NPTS,QSTACK(IOFFSX),QSTACK(IOFFSY))
*     Note: Routine GKCRCN ensures that NPTS is at least 2

*     Add in centre if pie and arc has ends
      IF(NPTSA .EQ. NPTS+1)THEN
          QSTACK(IOFFSX+NPTS) = XCEN
          QSTACK(IOFFSY+NPTS) = YCEN
      ENDIF

*     -- now output arc/circle

      IF (IOPT.EQ.JARC .OR. NPTS.EQ.2) THEN
         CALL GKMTXF(QWTOTT(1,KWKIX),NPTS,
     :                      QSTACK(IOFFSX),QSTACK(IOFFSY),
     :                      QSTACK(IOFFSX),QSTACK(IOFFSY))
         CALL GKLCLP(NPTS,QSTACK(IOFFSX),QSTACK(IOFFSY),
     :                  LSIMUL,SIMLEN,
     :                  QWCLXL(KWKIX),QWCLYB(KWKIX),
     :                  QWCLXR(KWKIX),QWCLYT(KWKIX),
     :                  LINSUB)
      ELSE
         CALL GKFILS(NPTSA,QSTACK(IOFFSX),QSTACK(IOFFSY),IFILSC,
     :                  LINSUB,ROSUB)
      ENDIF


      CALL GKSTDA(KREALS,IOFFST)
      DO 20 I=1,6
         QWTOTT(I,KWKIX) = TQCOPY(I)
   20 CONTINUE

 9999 CONTINUE
      END
