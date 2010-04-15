
*--------------------------------------------------------------------
      SUBROUTINE GK1ACC(IOPT, NRD, XW, YW)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*  Author:  KEVP
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Send GDP -1,-2,-3 or -4 to the external PostScript file.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*  ARGUMENTS
*  ---------
*      INP IOPT    - GDP option (-1,-2,-3 or -4)
*      INP NRD     - Integer number GDP points
*      INP XW      - Array of real x-coordinates of GDP points (WC)
*      INP YW      - Array of real y-coordinates of GDP points (WC)
*
      INTEGER IOPT, NRD
      REAL XW(*), YW(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*
*  ANGL1   - Angle to First Point (degrees)
*  ANGL2   - Angle to Last Point (degrees)
*  AXFM    - Transfromation from Circular GDP Coordinate System
*  BXFM    - Transfromation to Circular GDP Coordinate System
*  CHANGE  - Logical flag.
*  DPR     - Conversion factor (Degrees Per Radian)
*  DUMMY   - Dummy character, required by the buffering routine.
*  I       - Do loop index over total workstation transformation
*  IFASTY  - Workspace offset for current fill area style
*  IOFF    - Offset for stack.
*  IREM    - Dummy integer, required by the buffering routine.
*  ISTAT   - Status of Arc (has Ends, Full circle, Line)
*  RADIUS  - Arc Radius
*  REX,REY - Registration Points in Scaled Postscript Coordinates
*  RSCALE  - Scale factor for transformed registration points
*  S       - Character variable, via which chunks of PostScript are sent for
*            buffering.
*  THETA   - Turning Angle of Arc (Radians, Anticlockwise +ve)
*  TQCOPY  - Array to save original values of total workstation transformation
*  XCEN,
*   YCEN   - Centre of Arc Circle in Postscript Coordinates
*  XP,YP   - Coordinates of Points in Postscript Coordinates
*

*     Offsets in KWKDAT
      INTEGER   IFASTY
      PARAMETER (IFASTY=4)

*     Parameters (values of IOPT)
      INTEGER    JARC,    JCHORD,    JPIE,    JCIRC
      PARAMETER (JARC=-1, JCHORD=-2, JPIE=-3, JCIRC=-4)

*     Parameters (values of ISTAT)
      INTEGER    JENDS,   JFULL,   JLIN
      PARAMETER (JENDS=0, JFULL=1, JLIN=2)

*     small real
      REAL       SMALL
      PARAMETER (SMALL=1.0E-4)

*
      LOGICAL CHANGE
      CHARACTER S*100, DUMMY
      INTEGER I, IOFF, IREM, INTA(3), ISTAT
      REAL DPR, XP(3),YP(3)
      REAL ANGL1,ANGL2, RADIUS, THETA, XCEN,YCEN
      REAL AXFM(3,2), BXFM(3,2), REX(3),REY(3), RSCALE, TQCOPY(6)
*
*  ALGORITHM
*  ---------
*     The circular path is formed with transformed coordinates
*     if necessary. The centre is added of PIE (-3).
*
*     The path is sent as a curve for option ARC (-1)
*     or as a filled area as in GK1AFL for other options.
*
*     The postscript coordinate system is changed to one
*     in which the GDP is circular. The original coordinate
*     system is 'gsave'd and afterwards 'grestore'd.
*
*---------------------------------------------------------------------

*     Check if Curve Option is in range
      IF((IOPT .LT. -4) .OR. (IOPT .GT. -1)) GOTO 999
*     Check that number of points is correct for Curve Option.
      IF (.NOT.((NRD.EQ.3 .AND. IOPT.NE.JCIRC) .OR.
     :          (NRD.EQ.2 .AND. IOPT.EQ.JCIRC))
     :   )THEN
         KERROR = 100
         GOTO 999
      ENDIF
*
*     Calculate the number of Degrees Per Radian
      DPR = 45.0/ATAN(1.0)

*
*     Start from a new line in the external file
*
      CALL GKFOCO(KIOSN,DUMMY,IREM)


*     Update appropriate attributes
*
      IF(IOPT .EQ. JARC)THEN
*     Polyline attributes
        CALL GK1AST(KWPLCI(KWKIX),KWLNTY(KWKIX),QWLNWD(KWKIX))
      ELSE
*       Initialise change flag
        CHANGE = .FALSE.
*     Fill area attributes
*       Fill Area Style Index:
        IF(KWKDAT(IFASTY,KWKIX).NE.KWFASI(KWKIX)) THEN
           KWKDAT(IFASTY,KWKIX) = KWFASI(KWKIX)
           CHANGE = .TRUE.
        ENDIF
*       Set up the fill area attributes (style index),
*       if change has occurred.
        IF (CHANGE) THEN
           WRITE(S, 50) KWKDAT(IFASTY,KWKIX)
   50      FORMAT( I3, ' fastat')
           CALL GKFOCO(KIOPB, S(1:10), IREM)
           CALL GKFOCO(KIOSN,DUMMY,IREM)
        ENDIF
      ENDIF

*
*     Form the path for Postscript arc
*

*     Take a memory snapshot, so as not to clutter the memory with
*     temporaries.
      CALL GKFOCO(KIOPB,'save',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*     Prepare and postscript arc
*

*     Modify the coordinate system to take account of the
*     registration points.
*
*     Save original total workstation transformation
      DO 10 I=1,6
         TQCOPY(I) = QWTOTT(I,KWKIX)
   10 CONTINUE

*     Change coordinates to take account of registration points.
*     This is done to both the total workstation transformation
*     and the postscript coordinate transformation.
*     (The original coords are restored at the end of the routine.)
*
*     Convert registration points from WC to Postscript Coordinates
*     and rescale them uniformly.
      CALL GKTWD(3,QWRA(1),QWRA(4),REX,REY)
      RSCALE = 1.0/SQRT(REX(1)*REX(1)+REY(1)*REY(1))
      DO 20 I=1,3
         REX(I) = RSCALE*REX(I)
         REY(I) = RSCALE*REY(I)
   20 CONTINUE

*     Change the Postscript Coord System and workstation transformation
      CALL GKMTDV(REX,REY,AXFM)
      CALL GKMTIV(AXFM,BXFM)
      CALL GKMTML(BXFM,QWTOTT(1,KWKIX),QWTOTT(1,KWKIX))
      WRITE(S,90) AXFM(1,1),AXFM(1,2),AXFM(2,1),AXFM(2,2),
     :            AXFM(3,1),AXFM(3,2)
   90 FORMAT( '[', 6F10.5, ']concat')
      CALL GKFOCO(KIOPB,S(1:68),IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Transform the points to postscript coordinates
      CALL GKTWD(NRD,XW,YW,XP,YP)

*     Start Newpath
      CALL GKFOCO(KIOPB,'newpath',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Get the parameters required by the postscript arc operator
      IF(IOPT .EQ. JCIRC)THEN
         XCEN = XP(2)
         YCEN = YP(2)
         RADIUS = SQRT((XP(1)-XCEN)**2 + (YP(1)-YCEN)**2)
         THETA = 0.0
         ISTAT = JFULL
         ANGL1 = 0.0
         ANGL2 = 360.0
      ELSE
         CALL GKCRCE (XP,YP,XCEN,YCEN,RADIUS,THETA,ISTAT)
         ANGL1 = DPR*ATAN2(YP(1)-YCEN,XP(1)-XCEN)
         IF(ANGL1 .LT. 0.0)ANGL1 = ANGL1 + 360.0
         ANGL2 = DPR*ATAN2(YP(3)-YCEN,XP(3)-XCEN)
         IF(ANGL2 .LT. 0.0)ANGL2 = ANGL2 + 360.0
      ENDIF

      IF(ISTAT .EQ. JLIN)THEN
*        Collinear Points
         WRITE(S,99) XP(2),YP(2)
   99    FORMAT ( 2F9.4,' lineto')
         CALL GKFOCO(KIOPB,S(1:25),IREM)
      ELSE
*       Send out the arc operator
        IF(ISTAT .EQ. JFULL .OR. THETA .GE. 0.0)THEN
*         Anticlockwise (full circle is drawn anticlockwise)
          WRITE(S,100) XCEN,YCEN, RADIUS, ANGL1,ANGL2
  100     FORMAT( 5F9.4,' arc')
          CALL GKFOCO(KIOPB,S(1:49),IREM)
        ELSE
*         Clockwise
          WRITE(S,101) XCEN,YCEN, RADIUS, ANGL1,ANGL2
  101     FORMAT( 5F9.4,' arcn')
          CALL GKFOCO(KIOPB,S(1:50),IREM)
        ENDIF
      ENDIF
      CALL GKFOCO(KIOSN,DUMMY,IREM)

      IF(IOPT .EQ. JARC)THEN
*        Arc - Just Stroke
         CALL GKFOCO(KIOPB,' stroke',IREM)
      ELSE
*       Filled GDP
        IF(IOPT .EQ. JPIE)THEN
*       Add Centre to Path
          WRITE(S,99) XCEN,YCEN
          CALL GKFOCO(KIOPB,S(1:25),IREM)
          CALL GKFOCO(KIOSN,DUMMY,IREM)
        ENDIF
*
*       Call the fasoldo procedure to do solid fill.
*
        IF(KWFAIS(KWKIX).EQ.GSOLID)THEN
           CALL GKFOCO(KIOPB,' fasoldo', IREM)
           CALL GKFOCO(KIOSN,DUMMY,IREM)
        ELSEIF(KWFAIS(KWKIX).EQ.GPATTR)THEN

*
*       Deal with pattern here.
*
           CALL GK1APA
        ENDIF
      ENDIF

*
*     End GDP - do the restores to match the saves.
*               this will restore the original postscript coordinate
*               transformation
*
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'grestore',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'restore',IREM)

*     Restore the total workstation transformation
      DO 30 I=1,6
         QWTOTT(I,KWKIX) = TQCOPY(I)
   30 CONTINUE

  999 CONTINUE
      END
