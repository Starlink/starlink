C# IL>=a, OL>=0
      SUBROUTINE GKMTYP(N,X,Y,MTYPE,SIZE,FACT,XMIN,YMIN,XMAX,YMAX,LN)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     This routine takes each point, and for the given marker type
*     draws lines to simulate that marker using the nominal marker
*     size and marker size scale factor.
*
*  MAINTENANCE LOG
*  ---------------
*     18/02/83  AS    Original version stabilized
*      5/04/83  AS    Change arguments for GKLCLP
*     21/04/83  AS    Move external declaration
*     16/01/84  AS    Change definition of dot marker
*     20/08/85  RMK   Changed definition of circle marker to have 8 sides
*                     rather than 4 (S75). Reduced space used in storage of
*                     marker definitions.
*      6/11/85  DRJF  Validation of MARKER size introduced
*
*  EXTERNALS
*  ---------
*
      EXTERNAL LN
*
*  ARGUMENTS
*  ---------
*     INP N     - size of arrays X,Y
*     INP X,Y   - marker points in DC
*     INP MTYPE - marker type
*     INP SIZE  - nominal marker size in DC
*     INP FACT  - marker size scale factor
*     INP XMIN  -
*     INP YMIN  - clipping rectangle
*     INP XMAX  -
*     INP YMAX  -
*     INP LN    - routine to call, - device dependent routine
*                 which draws a polyline on the device.
*
      INTEGER N,MTYPE
      REAL X(N),Y(N),SIZE,FACT,XMIN,YMIN,XMAX,YMAX
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwdt.cmn'
      INCLUDE '../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     MAXTYP - maximum number of marker types.
*     MAXPNT - maximum number of points per marker, ie the maximum
*              number of lines for any marker * 2.
*     EXTENT - maximum fractional distance from marker point. It is
*              used as a quick way of testing if a marker needs to be
*              clipped.
*     NUMPTS - array which holds the number of points for each marker.
*     NSTART - array holding start point in POSX,POSY for each marker type.
*     POSX,POSY - hold the relative points for each marker type.
*     AMULT  - multiplication factor for absolute marker size.
*     FACTOR - frequently used EXTENT*AMULT
*     XA,YA  - coordinates of line that needs to be drawn.
*     XMULT,YMULT - relative points for current marker in DC
*     CLIP   - True if marker is partially outside clipping
*              rectangle and needs to be clipped.
*
      INTEGER MAXTYP,MAXPNT,NDO,I,J
      REAL EXTENT
      PARAMETER (MAXTYP=5, MAXPNT=16, EXTENT=0.5)
      INTEGER NUMPTS(MAXTYP), NSTART(MAXTYP)
      REAL POSX(38), POSY(38)
      REAL AMULT,FACTOR,XA(2),YA(2),XMULT(MAXPNT),YMULT(MAXPNT)
      REAL XTEMP1,XTEMP2,YTEMP1,YTEMP2
      LOGICAL CLIP

*     Marker types are: . + * o x
      DATA NUMPTS/8,4,6,16,4/
      DATA NSTART/1,9,13,19,35/
      DATA POSX/-0.04, 0.04, 0.04, 0.04, 0.04, -0.04, -0.04, -0.04,
     :          -0.5,  0.5,  0.0,  0.0,
     :          -0.5,  0.5, -0.25, 0.25,-0.25,  0.25,
     :          -0.5, -0.2, -0.2,  0.2,  0.2,   0.5,   0.5,   0.5,
     :           0.5,  0.2,  0.2, -0.2, -0.2,  -0.5,  -0.5,  -0.5,
     :          -0.5,  0.5, -0.5,  0.5/
      DATA POSY/-0.04,-0.04,-0.04, 0.04, 0.04,  0.04,  0.04, -0.04,
     :           0.0,  0.0, -0.5,  0.5,
     :           0.0,  0.0,  0.5, -0.5, -0.5,   0.5,
     :          -0.2, -0.5, -0.5, -0.5, -0.5,  -0.2,  -0.2,   0.2,
     :           0.2,  0.5,  0.5,  0.5,  0.5,   0.2,   0.2,  -0.2,
     :          -0.5,  0.5,  0.5, -0.5/
*
*  COMMENTS
*  --------
*     To add another markertype:
*        Increase MAXTYP by 1.
*        If, in the new marker definition, any distance from the centre is
*        greater than EXTENT, then increase EXTENT.
*        Add the new number of lines*2 (ie. the number of new data points)
*        at the end of data NUMPTS.
*        Increase the lengths of POSX,POSY by the number of new data points.
*        Add the starting point for the new marker type to the end of NSTART.
*        Add the definition of the lines to data POSX and POSY.
*
*---------------------------------------------------------------------


* Work out AMULT. Marker type 1 is a special case since it is
* always displayed as the smallest displayable dot.

      IF (MTYPE .EQ. 1) THEN
        AMULT = SIZE
      ELSE
        AMULT = SIZE*FACT
      ENDIF

* Test to see if MARKER size is valid.
      IF (AMULT.LT.QMNMKS(KWKIX)) THEN
* MARKER size smaller than minimum size allowed. Set MARKER
* size to minimum size allowed.
        AMULT=QMNMKS(KWKIX)
      ELSE IF (AMULT.GT.QMXMKS(KWKIX)) THEN
* MARKER size larger than maximum size allowed. Set MARKER
* size to maximum size allowed.
        AMULT=QMXMKS(KWKIX)
      END IF

      FACTOR = EXTENT*AMULT
      NDO = NUMPTS(MTYPE)
      DO 10 I=1,NDO
        XMULT(I) = POSX(NSTART(MTYPE)+I-1)*AMULT
        YMULT(I) = POSY(NSTART(MTYPE)+I-1)*AMULT
   10 CONTINUE


      DO 40 I=1,N

* Check if marker inside clipping rectangle.

        IF ( X(I).GE.XMIN .AND. X(I).LE.XMAX .AND.
     :       Y(I).GE.YMIN .AND. Y(I).LE.YMAX ) THEN

* Now check if marker is partially outside and needs to be clipped.

          CLIP = .FALSE.
          XTEMP1 = X(I) + FACTOR
          XTEMP2 = X(I) - FACTOR
          YTEMP1 = Y(I) + FACTOR
          YTEMP2 = Y(I) - FACTOR
          IF (XTEMP1.GT.XMAX .OR. XTEMP2.LT.XMIN .OR.
     :        YTEMP1.GT.YMAX .OR. YTEMP2.LT.YMIN) CLIP = .TRUE.

* Do over the number of lines for the given marker. Work out
* coordinates for the beginning and endpoints. If CLIP is
* true and the marker is partially outside then clip line.

          IF (.NOT.CLIP) THEN
            DO 20 J=1,NDO,2
              XA(1) = X(I) + XMULT(J)
              YA(1) = Y(I) + YMULT(J)
              XA(2) = X(I) + XMULT(J+1)
              YA(2) = Y(I) + YMULT(J+1)
              CALL LN(2,XA,YA)
   20       CONTINUE
          ELSE
            DO 30 J=1,NDO,2
              XA(1) = X(I) + XMULT(J)
              YA(1) = Y(I) + YMULT(J)
              XA(2) = X(I) + XMULT(J+1)
              YA(2) = Y(I) + YMULT(J+1)
              CALL GKLCLP(2,XA,YA,.FALSE.,0.0,XMIN,YMIN,XMAX,YMAX,LN)
   30       CONTINUE
          ENDIF

        ENDIF

   40 CONTINUE

      END
