	SUBROUTINE CONTOUR_ANNOT(   CONTOUR_NUMBER,
     :	                            CONTOUR_BASE,
     :	                            CONTOUR_INTERVAL,
     :	                            CONTOUR_TITLE,
     :	                            CONTOUR_MAGNIF,
     :	                            STATUS)


* History
* 20-Jul-1994 Changed LIB$ calls to CHR_ (SKL@JACH)
* 26-OCT-1994 Changed MAGNIF from INT to REAL (SKL@JACH)
*

	IMPLICIT NONE

	INCLUDE 'PLT2DCOM'
        INCLUDE 'SAE_PAR'
        INCLUDE 'CHR_ERR'

        INTEGER NCHAR
	REAL CONTOUR_MAGNIF
	INTEGER CONTOUR_NUMBER
	INTEGER L1
	INTEGER STATUS

	REAL CONTOUR_BASE
	REAL TEXT_HT
	REAL CONTOUR_INTERVAL
	REAL X1
	REAL Y1

	CHARACTER CONTOUR_TITLE*40
	CHARACTER*1 CONTOUR_TYPE
	CHARACTER CCN*3
	CHARACTER CNX*3
	CHARACTER CNY*3
	CHARACTER OUT_STRING*70
	CHARACTER CONLEVELS*80
	CHARACTER DUMMY*132
	CHARACTER BLUE_SWITCH*40

* ==============================================================================

* set height of text

	TEXT_HT = 10.0*NY/64*CONTOUR_MAGNIF/6.0
	CALL SGS_SHTX( TEXT_HT)

* get the blue switch for contour title and base stuff

	CALL PAR_GET0C( 'BLUE_SWITCH', BLUE_SWITCH, STATUS)

* set text positioning pointer to centre,centre

	CALL SGS_STXJ( 'CC')

* position for title and plot'em

	X1 = IM_XST + ( IM_XEN - IM_XST)/2.0
	Y1 = IM_YEN + 3.0*TEXT_HT*1.5

	OUT_STRING = CONTOUR_TITLE
!	OUT_STRING = 'Contour Map - ' // CONTOUR_TITLE

	IF( DEVICE_NAME .EQ. 'T5688') THEN

	  IF( X1 .GT. 0.0 .AND. X1 .LT. MAX_X/2.0 .AND.
     :	      Y1 .GT. 0.0 .AND. Y1 .LT. MAX_Y/2.0) THEN

	    IF( BLUE_SWITCH( 1:1) .NE. 'N') THEN
	      CALL SGS_BTEXT( X1, Y1)
	      CALL SGS_ATXL( OUT_STRING)
	    END IF

	  END IF

	ELSE

	  IF( BLUE_SWITCH( 1:1) .NE. 'N') THEN
	    CALL SGS_BTEXT( X1, Y1)
	    CALL SGS_ATXL( OUT_STRING)
	  END IF

	END IF

* position for picture size and contour number and plot'em

	X1 = IM_XST + ( IM_XEN - IM_XST)/2.0
	Y1 = ( IM_YEN + 1.5*TEXT_HT*1.5)

        CALL CHR_ITOC( NX, CNX, NCHAR )
        CALL CHR_ITOC( NY, CNY, NCHAR )
        CALL CHR_ITOC( CONTOUR_NUMBER, CCN, NCHAR )

	OUT_STRING = 'Size = ' // CNX // ',' // CNY // ' : Contours = ' //
     :	CCN

	IF( DEVICE_NAME .EQ. 'T5688') THEN

	  IF( X1 .GT. 0.0 .AND. X1 .LT. MAX_X/2.0 .AND.
     :	      Y1 .GT. 0.0 .AND. Y1 .LT. MAX_Y/2.0) THEN

	    IF( BLUE_SWITCH( 1:1) .NE. 'N') THEN
	      CALL SGS_BTEXT( X1, Y1)
	      CALL SGS_ATXL( OUT_STRING)
	    END IF

	  END IF

	ELSE

	  IF( BLUE_SWITCH( 1:1) .NE. 'N') THEN
	    CALL SGS_BTEXT( X1, Y1)
	    CALL SGS_ATXL( OUT_STRING)
	  END IF

	END IF

* get contour type and test if it is C or not ...

	CALL PAR_GET0C( 'CONTOUR_TYPE', CONTOUR_TYPE, STATUS)

	IF( CONTOUR_TYPE .NE. 'C') THEN

* position for contour base and interval and plot'em

	  X1 = IM_XST + ( IM_XEN - IM_XST)/2.0
	  Y1 = ( IM_YST - 2.5*TEXT_HT*1.5)

	  IF( DEVICE_NAME .EQ. 'T5688') THEN

	    IF( X1 .GT. 0.0 .AND. X1 .LT. MAX_X/2.0 .AND.
     :	        Y1 .GT. 0.0 .AND. Y1 .LT. MAX_Y/2.0) THEN

	      IF( BLUE_SWITCH( 1:1) .NE. 'N') THEN
	        CALL SGS_BTEXT( X1, Y1)
	        CALL SGS_ATXL( 'Base = ')
	        CALL SGS_ATXR( CONTOUR_BASE, -8, 1)
	        CALL SGS_ATXL( ' : Step = ')
	        CALL SGS_ATXR( CONTOUR_INTERVAL, -8, 1)
	      END IF

	    END IF

	  ELSE

	    IF( BLUE_SWITCH( 1:1) .NE. 'N') THEN
	      CALL SGS_BTEXT( X1, Y1)
	      CALL SGS_ATXL( 'Base = ')
	      CALL SGS_ATXR( CONTOUR_BASE, -8, 1)
	      CALL SGS_ATXL( ' : Step = ')
	      CALL SGS_ATXR( CONTOUR_INTERVAL, -8, 1)
	    END IF

	  END IF

	ELSE

* position for contour list and plot'em

	  X1 = IM_XST + ( IM_XEN - IM_XST)/2.0
	  Y1 = ( IM_YST - 2.5*TEXT_HT*1.5)

	  CALL PAR_GET0C( 'CONTOUR_LEVELS', CONLEVELS, STATUS)
          CALL CHR_CLEAN( CONLEVELS )
          L1=0
	  CALL CHR_APPND( CONLEVELS, CONLEVELS, L1)

	  IF( DEVICE_NAME .EQ. 'T5688') THEN

	    IF( X1 .GT. 0.0 .AND. X1 .LT. MAX_X/2.0 .AND.
     :	        Y1 .GT. 0.0 .AND. Y1 .LT. MAX_Y/2.0) THEN

	      IF( BLUE_SWITCH( 1:1) .NE. 'N') THEN
	        CALL SGS_BTEXT( X1, Y1)
	        DUMMY = 'Contour Levels = '//CONLEVELS( 1:L1)
                CALL CHR_CLEAN( DUMMY )
                L1 = 0
	        CALL CHR_APPND( DUMMY, DUMMY, L1)
	        CALL SGS_ATXL( DUMMY( 1:L1))
	      END IF

	    END IF

	  ELSE

	    IF( BLUE_SWITCH( 1:1) .NE. 'N') THEN
	      CALL SGS_BTEXT( X1, Y1)
	      DUMMY = 'Contour Levels = '//CONLEVELS( 1:L1)
              CALL CHR_CLEAN( DUMMY )
              L1 = 0
              CALL CHR_APPND( DUMMY, DUMMY, L1)
	      CALL SGS_ATXL( DUMMY( 1:L1))
	    END IF

	  END IF

	END IF

* flush SGS buffer of all output

	CALL SGS_FLUSH

	END
