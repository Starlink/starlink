	SUBROUTINE CONTOUR_MAP( IMAGE, BASE, STEP, NUMCON, MAGNIF, AXRAT)

* CONTOUR plotting routine :

* HISTORY
*   26-Oct-1994 Changed MAGNIF from INT to REAL (SKL@JACH)
*

	IMPLICIT NONE

	INCLUDE 'PLT2DCOM'

	INTEGER IC
	INTEGER IT
	INTEGER K
	INTEGER KX
	INTEGER L
	INTEGER LY
	INTEGER M
	REAL MAGNIF
	INTEGER N
	INTEGER NUMCON

	REAL AXRAT
	REAL B1
	REAL B2
	REAL B( 5)
	REAL BASE
	REAL BMAX
	REAL BMIN
	REAL BX
	REAL BY
	REAL CX( 4)
	REAL CY( 4)
	REAL D( 4)
	REAL DA
	REAL DB
	REAL DX( 4)
	REAL DY( 4)
	REAL HT1
	REAL HT
	REAL HTMAX
	REAL HTMIN
	REAL IMAGE( NX, NY)
	REAL P
	REAL RAT
	REAL STEP
	REAL X( 5)
	REAL Y( 5)

* set data

	DATA CX/0.0, 1.0, 1.0, 0.0/
	DATA CY/0.0, 0.0, 1.0, 1.0/
	DATA DX/1.0, 0.0, -1.0, 0.0/
	DATA DY/0.0, 1.0, 0.0, -1.0/

* reset STEP if zero

	IF( STEP. EQ. 0.0) THEN
	  STEP = 1.0
	END IF

* set heights of contour and max,min scan range

	HT1 = BASE - STEP
	HTMAX = BASE + ABS( STEP)*FLOAT( NUMCON - 1)
	HTMIN = BASE - ABS( STEP)*FLOAT( NUMCON - 1)

* take ( 2x2) squares of gridpoints from array a

	DO 16 LY = 1, (NY - 1)
!	DO 16 LY = 1, (NY - 2)

	  BY = FLOAT( LY)

	  DO 15 KX = 1, (NX - 1)
!	  DO 15 KX = 1, (NX - 2)

	    BX = FLOAT( KX)

* B( 1 - 4) contain corners of ( 2x2) grid

	    B( 1) = IMAGE( KX, LY)
	    B( 2) = IMAGE( KX+1, LY)
	    B( 3) = IMAGE( KX+1, LY+1)
	    B( 4) = IMAGE( KX, LY+1)

* BMIN = lowest corner value
* BMAX = highest corner value

	    BMIN = AMIN1( B( 1), B( 2), B( 3), B( 4))

	    IF ( BMIN. GT. HTMAX) GOTO  15

	    BMAX = AMAX1( B( 1), B( 2), B( 3), B( 4))

	    IF ( BMAX. LT. HTMIN) GOTO  15

	    B( 5) = B( 1)
	    HT = HT1

* step thru contour heights up to htmax.

 14         HT = HT + STEP

	    IF( HT. GT. HTMAX) GOTO 15
	    IF( HT. LT. HTMIN) GOTO 15
	    IF( HT. LE. BMIN. AND. STEP. GT. 0.0) GOTO 14
	    IF( HT. GT. BMAX. AND. STEP. GT. 0.0) GOTO 15
	    IF( HT. GT. BMAX. AND. STEP. LT. 0.0) GOTO 14
	    IF( HT. LE. BMIN. AND. STEP. LT. 0.0) GOTO 15

	    IT = 0

* interpolate intersection points of contour
* with borders of current ( 2x2) square

	    DO 11 N = 1, 4

	      B1 = B( N)
	      B2 = B( N + 1)

	      IF( B1. LT. HT. AND. B2. LT. HT) GOTO 11
	      IF( B1. GE. HT. AND. B2. GE. HT) GOTO 11

	      IT = IT + 1
	      P = ( HT - B1)/( B2 - B1)
	      X( IT) = BX + CX( N) + DX( N)*P
	      Y( IT) = BY + CY( N) + DY( N)*P

 11         CONTINUE

* join intersection points in current ( 2x2) square

	    CALL SGS_BPOLY( IM_XST+(X( 1)-0.5)*MAGNIF,
     :	                    IM_YST+(Y( 1)-0.5)*MAGNIF*AXRAT)

	    IF ( IT. NE. 2) GOTO  12

* just two intersections : they are joined

	    CALL SGS_APOLY( IM_XST+(X( 2)-0.5)*MAGNIF,
     :	                    IM_YST+(Y( 2)-0.5)*MAGNIF*AXRAT)

	    GOTO  14

 12       CONTINUE

* four intersections
* only two non - crossing lines will be drawn.
* tests are made to determine pairing of intersections.
* test one.   pair intersections one way if results
* in total line - length saving by a factor of 4 or
* more over other pairing.

          X( 5) = X( 1)
	  Y( 5) = Y( 1)

	  DO 23 N = 1, 4

	    DA = X( N) - X( N + 1)
	    DB = Y( N) - Y( N + 1)
	    D( N) = SQRT( DA*DA + DB*DB)

 23       CONTINUE

	  IF( D( 2) + D( 4). EQ. 0.0) GOTO 22

	  RAT = ( D( 1) + D( 3))/( D( 2) + D( 4))

	  IF( RAT. GT. 4.0) GOTO 22
	  IF( RAT. LT. 0.25) GOTO 21

* test two.
* the ( 4x4) square centred on the ( 2x2) square
* is examined : it = no of gridpoints in the ( 4x4)
* square; ic = no of these gridpoints below current
* contour level.
* if majority of gridpoints are above contour level,
* two lines cut diagonal between two highest
* gridpoints of ( 2x2) square; otherwise two
* lines cut diagonal between two lowest gridpoints.

	  IC = 0
	  IT = 0

	  DO 19 N = 1, 4

	    K = KX + N - 2

	    IF ( K. LT. 1. OR.  K. GT. NX) GOTO  19

	    DO 18 M = 1, 4

	      L = LY + M - 2

	      IF ( L. LT. 1. OR.  L. GT. NY) GOTO  18

	      IF ( IMAGE( K, L). LT. HT) IC = IC + 1

	      IT = IT + 1

 18         CONTINUE

 19       CONTINUE

	  IF( IC. NE. IT/2) GOTO 24

	  IF( RAT. LT. 1.0) GOTO 21

	  GOTO 22

 24       IF( IC.GT.IT/2) GOTO 20

	  IF ( B( 1). GT. B( 2)) GOTO  22

	  GOTO  21

 20       IF ( B( 1). LT. B( 2)) GOTO  22

 21       CALL SGS_APOLY( IM_XST+(X( 2)-0.5)*MAGNIF,
     :	                  IM_YST+(Y( 2)-0.5)*MAGNIF*AXRAT)
	  CALL SGS_BPOLY( IM_XST+(X( 4)-0.5)*MAGNIF,
     :	                  IM_YST+(Y( 4)-0.5)*MAGNIF*AXRAT)
	  CALL SGS_APOLY( IM_XST+(X( 3)-0.5)*MAGNIF,
     :                    IM_YST+(Y( 3)-0.5)*MAGNIF*AXRAT)

	  GOTO  14

 22       CALL SGS_APOLY( IM_XST+(X( 4)-0.5)*MAGNIF,
     :	                  IM_YST+(Y( 4)-0.5)*MAGNIF*AXRAT)
	  CALL SGS_BPOLY( IM_XST+(X( 3)-0.5)*MAGNIF,
     :	                  IM_YST+(Y( 3)-0.5)*MAGNIF*AXRAT)
	  CALL SGS_APOLY( IM_XST+(X( 2)-0.5)*MAGNIF,
     :	                  IM_YST+(Y( 2)-0.5)*MAGNIF*AXRAT)

	  GOTO  14

 15     CONTINUE

 16     CONTINUE

	END
