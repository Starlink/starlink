	SUBROUTINE POLAX_ANNOT( POLTIT, POL_MAGNIF, VECSLEN, STATUS)

* HISTORY
*  22-Jul-1994 Changed LIB$, STR$ to CHR_ (SKL@JACH)
* 26-Oct-1994 Changed MAGNIF from INT to REAL (SKL@JACH)
*

	IMPLICIT NONE

        INCLUDE 'SAE_PAR'
        INCLUDE 'CHR_ERR'
	INCLUDE 'PLT2DCOM'

	REAL POL_MAGNIF
	INTEGER L1
        INTEGER NCHAR
	INTEGER STATUS

	REAL TEXT_HT
	REAL VECSLEN
	REAL X1
	REAL X2
	REAL Y1

	CHARACTER*(*) POLTIT
	CHARACTER CNX*3
	CHARACTER CNY*3
	CHARACTER OUT_STRING*80
	CHARACTER BLUE_SWITCH*40

* ==============================================================================

* set height of text

	TEXT_HT = 10.0*REAL(NY)/64.0*POL_MAGNIF/6.0
	CALL SGS_SHTX( TEXT_HT)

* get the switch for ploting annotation or not

	CALL PAR_GET0C( 'BLUE_SWITCH', BLUE_SWITCH, STATUS)

* set text positioning pointer to centre,centre

	CALL SGS_STXJ( 'CC')

* position for title and plot'em

	X1 = IM_XST + ( IM_XEN - IM_XST)/2.0
	Y1 = IM_YEN + 3.0*TEXT_HT*1.5

        CALL CHR_CLEAN( POLTIT )
        L1 = 0
	CALL CHR_APPND( POLTIT, POLTIT, L1)

	OUT_STRING = POLTIT( 1:L1)
!	OUT_STRING = 'Polarization Vector Map - ' // POLTIT( 1:L1)

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

* position for picture size and plot'em

	X1 = IM_XST + ( IM_XEN - IM_XST)/2.0
	Y1 = ( IM_YEN + 1.5*TEXT_HT*1.5)

        CALL CHR_ITOC( NX, CNX, NCHAR )
        CALL CHR_ITOC( NY, CNY, NCHAR )

	OUT_STRING = 'Size = ' // CNX // ',' // CNY

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


	X1 = IM_XST + ( IM_XEN - IM_XST)/2.0
	Y1 = ( IM_YST - 2.0*TEXT_HT*1.5)

	IF( DEVICE_NAME .EQ. 'T5688') THEN

	  IF( X1 .GT. 0.0 .AND. X1 .LT. MAX_X/2.0 .AND.
     :	      Y1 .GT. 0.0 .AND. Y1 .LT. MAX_Y/2.0) THEN

	  END IF

	ELSE

	END IF

* position for vector scale length and plot'em

	X1 = IM_XST + ( IM_XEN - IM_XST)/2.0
	Y1 = IM_YST - TEXT_HT*4.0

	OUT_STRING = '100% Vector'

	CALL SGS_BTEXT( X1, Y1)
	CALL SGS_ATXL( OUT_STRING)

	X1 = IM_XST + ( IM_XEN - IM_XST)/2.0 - VECSLEN*POL_MAGNIF/2.0
	X2 = X1 + VECSLEN*POL_MAGNIF
	Y1 = IM_YST - TEXT_HT*5.5

	CALL SGS_BPOLY( X1, Y1)
	CALL SGS_APOLY( X2, Y1)

* flush SGS buffer of all output

	CALL SGS_FLUSH

	END
