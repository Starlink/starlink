
*+  DIST - calculates distance between two points in an image

	SUBROUTINE DIST ( STATUS )

* Description :
*
*
* Invocation :
*
*     CALL DIST ( STATUS )
*
* Parameters :
*
* Method :
*
* Bugs :
*
*     None known.
*
* Authors :
*
*     Colin Aspin ROE ( REVA::CAA )
*
* History :
*
*     25-04-87 : First implementation (REVA::CAA)
*     24-Jun-1994 Changed LIB$SIGNAL on entry error to ERR_REP (SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE			! no default typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'		! SSE global definitions

* Status :

	INTEGER  STATUS			! global status parameter

* Local Constants :

* Local variables :

	INTEGER XPOS_1
	INTEGER YPOS_1
	INTEGER XPOS_2
	INTEGER YPOS_2

	REAL DEC
	REAL DEC_POS1( 3)
	REAL DEC_POS2( 3)
	REAL DISTANCE
	REAL PLATE_SCALE
	REAL POSITION_ANGLE
	REAL RA
	REAL RA_POS1( 3)
	REAL RA_POS2( 3)
	REAL XSEP
	REAL YSEP

*-
*      check status on entry - return if not o.k.

	IF ( STATUS .NE. SAI__OK ) THEN

	   CALL ERR_REP( 'ENTRY', 'Error on DIST entry', STATUS )

	   RETURN

	END IF

*      get the two positions, the plate scale and the ra,dec to be used

	CALL PAR_GET0I( 'XPOS1', XPOS_1, STATUS)
	CALL PAR_GET0I( 'YPOS1', YPOS_1, STATUS)

	CALL PAR_GET0I( 'XPOS2', XPOS_2, STATUS)
	CALL PAR_GET0I( 'YPOS2', YPOS_2, STATUS)

	CALL PAR_GET0R( 'PLATE_SCALE', PLATE_SCALE, STATUS)

	CALL PAR_GET0R( 'RA', RA, STATUS)
	CALL PAR_GET0R( 'DEC', DEC, STATUS)

*      call subroutine to convert real ra/dec to position

	CALL RADEC_CONVERT( RA, DEC, RA_POS1, DEC_POS1)

*      calculate the distance between the two points in arcseconds

	XSEP = ( XPOS_2 - XPOS_1)*PLATE_SCALE
	YSEP = ( YPOS_2 - YPOS_1)*PLATE_SCALE

	DISTANCE = SQRT( XSEP**2 + YSEP**2)

	IF( YSEP .GT. 0.0) THEN

	  POSITION_ANGLE = ATAN( ABS( XSEP)/ABS( YSEP))*180.0/3.1415927

	ELSE

	  POSITION_ANGLE = 90.0

	END IF

*      correct positionn angle for the quadrant

	IF( XSEP .GT. 0.0 .AND. YSEP .GT. 0.0) THEN

	  POSITION_ANGLE = POSITION_ANGLE

	ELSE IF( XSEP .LT. 0.0 .AND. YSEP .GT. 0.0) THEN

	  POSITION_ANGLE = POSITION_ANGLE + 270.0

	ELSE IF( XSEP .LT. 0.0 .AND. YSEP .LT. 0.0) THEN

	  POSITION_ANGLE = POSITION_ANGLE + 180.0

	ELSE IF( XSEP .GT. 0.0 .AND. YSEP .LT. 0.0) THEN

	  POSITION_ANGLE = POSITION_ANGLE + 90.0

	END IF

*      call subroutine to add on the offsets to position 2

	CALL RADEC_NEW( RA_POS1, DEC_POS1, XSEP, YSEP, RA_POS2, DEC_POS2)

	RA_POS1( 3) = REAL( IFIX( RA_POS1( 3)*100 + 0.5))/100.0
	DEC_POS1( 3) = REAL( IFIX( DEC_POS1( 3)*100 + 0.5))/100.0

	RA_POS2( 3) = REAL( IFIX( RA_POS2( 3)*100 + 0.5))/100.0
	DEC_POS2( 3) = REAL( IFIX( DEC_POS2( 3)*100 + 0.5))/100.0

*      tell user the result

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

	CALL MSG_SETR( 'DISTANCE', DISTANCE)

	CALL MSG_OUT( 'MESSAGE',
     :	  'Distance between point 1 and point 2 is ^DISTANCE arcseconds',
     :	  STATUS)

	CALL MSG_SETR( 'POSITION', POSITION_ANGLE)

	CALL MSG_OUT( 'MESSAGE',
     :	  'Position angle from N through E is ^POSITION degrees',
     :	  STATUS)

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

	CALL MSG_SETR( 'RAH', RA_POS1( 1))
	CALL MSG_SETR( 'RAM', RA_POS1( 2))
	CALL MSG_SETR( 'RAS', RA_POS1( 3))

	CALL MSG_OUT( 'MESSAGE',
     :	  'Position 1 has a RA  of ^RAH hrs,  ^RAM mins, ^RAS secs',
     :	  STATUS)

	CALL MSG_SETR( 'DECD', DEC_POS1( 1))
	CALL MSG_SETR( 'DECM', DEC_POS1( 2))
	CALL MSG_SETR( 'DECS', DEC_POS1( 3))

	CALL MSG_OUT( 'MESSAGE',
     :	  'Position 2 has a DEC of ^DECD degs, ^DECM mins, ^DECS secs',
     :	  STATUS)

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

	CALL MSG_SETR( 'RAH', RA_POS2( 1))
	CALL MSG_SETR( 'RAM', RA_POS2( 2))
	CALL MSG_SETR( 'RAS', RA_POS2( 3))

	CALL MSG_OUT( 'MESSAGE',
     :	  'Position 2 has a RA  of ^RAH hrs,  ^RAM mins, ^RAS secs',
     :	  STATUS)

	CALL MSG_SETR( 'DECD', DEC_POS2( 1))
	CALL MSG_SETR( 'DECM', DEC_POS2( 2))
	CALL MSG_SETR( 'DECS', DEC_POS2( 3))

	CALL MSG_OUT( 'MESSAGE',
     :	  'Position 2 has a DEC of ^DECD degs, ^DECM mins, ^DECS secs',
     :	  STATUS)

	END
