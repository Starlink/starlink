C# IL>=a, OL=0
      SUBROUTINE GKXBCL(XBOX,YBOX,XMIN,XMAX,YMIN,YMAX,ICNT)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     determine whether a box is clipped, if clipped, determine whether
*     it is partial or total, the return code in each case is 0 (not
*     clipped), 4 (totally clipped), 1-3 (partially clipped).
*
*  MAINTENANCE LOG
*  ---------------
*     10/03/83    FY  Original version stabilized
*     20/04/83    FY  splilt arrays with mixed x&y points
*
*  ARGUMENTS
*  ---------
*     INP XBOX   coordinates of the 4 corners of box
*     INP YBOX
*     INP XMIN   min. of x of clip rect.
*     INP XMAX   max. of x of clip rect.
*     INP YMIN   min. of y of clip rect.
*     INP YMAX   max. of y of clip rect.
*     OUT ICNT   indicate how much box is clipped.
*
      REAL XBOX(4),YBOX(4),XMIN,XMAX,YMIN,YMAX
      INTEGER ICNT
*
*  LOCALS
*  ------
*     X1      x component of one end of line
*     X2      x component of other end of line
*     Y1      y component of one end of line
*     Y2      y component of other end of line
*     X       temp. var.
*     Y       temp var.
*     IX1     clip status of X1
*     IX2     clip status of X2
*     IY1     clip status of Y1
*     IY2     clip status of Y2
*
      REAL X1,X2,Y1,Y2,X,Y
      INTEGER IX1,IX2,IY1,IY2,I
*
*---------------------------------------------------------------------


      ICNT = 4

      DO 100 I = 1,4
        IF ( (XBOX(I).LE.XMAX) .AND. (XBOX(I).GE.XMIN) .AND.
     :       (YBOX(I).LE.YMAX) .AND. (YBOX(I).GE.YMIN) )ICNT = ICNT - 1
100   CONTINUE

      IF (ICNT .NE. 4 ) GOTO 400

* otherwise test no lines intersect the clip rect.
* it will be sufficient if we can find one.

* use the following scheme to classify points (similar to that described
* in Newman's book, but with different number here).

*        1,1       3,1                 2,1
*        1,3       0,0                 2,6
*        1,2       6,2                 2,2

* we know that our points will not be in (0,0) the loop started at 200
* has to be repeated upto 4 times if none of the lines of BOX intersects
* the clip rectangle.

      X1 = XBOX(4)
      Y1 = YBOX(4)

* classify (X1,Y1)

      IX1 = 0
      IY1 = 0

      IF (X1 .LE. XMIN) THEN
        IX1 = 1
        IY1 = 3
      ELSEIF (X1 .GE. XMAX) THEN
        IX1 = 2
        IY1 = 6
      ENDIF

      IF (Y1 .LE. YMIN) THEN
        IY1 = 2
      ELSEIF (Y1 .GE. YMAX) THEN
        IY1 = 1
      ENDIF

      IF (IX1 .EQ. 0 ) IX1 = 3*IY1

      I = 1

  200 CONTINUE

      X2 = XBOX(I)
      Y2 = YBOX(I)
      IX2 = 0
      IY2 = 0

* classify (X2,Y2)

      IF (X2 .LE. XMIN) THEN
        IX2 = 1
        IY2 = 3
      ELSEIF (X2 .GE. XMAX) THEN
        IX2 = 2
        IY2 = 6
      ENDIF

      IF (Y2 .LE. YMIN) THEN
        IY2 = 2
      ELSEIF (Y2 .GE. YMAX) THEN
        IY2 = 1
      ENDIF

      IF (IX2 .EQ. 0) IX2 = 3*IY2

* hopefully it is trivial to reject or accept

      IF ((IX1 .EQ. IX2) .OR. (IY1 .EQ. IY2)) GOTO 250
      IF (((IX1+IX2) .GE. 9) .OR. ((IY1+IY2) .GE. 9)) GOTO 300

* have to find intersection points

* case 1 : x = xmin

      Y = Y1 + (Y2-Y1)*(XMIN-X1)/(X2-X1)
      IF ((Y .LE. YMAX) .AND. (Y .GE. YMIN)) GOTO 300

* case 2 : x = xmax

      Y = Y1 + (Y2-Y1)*(XMAX-X1)/(X2-X1)
      IF ((Y .LE. YMAX) .AND. (Y .GE. YMIN)) GOTO 300

* case 3 : y = ymin

      X = X1 + (X2 - X1)*(YMIN-Y1)/(Y2-Y1)
      IF ((X .LE. XMAX) .AND. (X .GE. XMIN)) GOTO 300

* case 4 : y = ymax

      X = X1 + (X2-X1)*(YMAX-Y1)/(Y2-Y1)
      IF ((X .LE. XMAX) .AND. (X .GE. XMIN)) GOTO 300

  250 CONTINUE

* the line is completely outside the clip rectangle
* test next line of BOX (if any)

      I = I + 1
      IF (I .GT. 4 ) GOTO 400

      X1 = X2
      Y1 = Y2
      IX1 = IX2
      IY1 = IY2
      GOTO 200

  300 CONTINUE

* at least one line of BOX is within clip rect.
* fix value of ICNT to anything but 0 or 4

      ICNT = 1

  400 CONTINUE
      END
