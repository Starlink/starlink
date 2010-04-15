
4	SUBROUTINE CRECOLT( STATUS)

* Description : Subroutine to create a colour table from user input colours
*	        at various positions in the colour table


* History
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     24-JUN-1994  Changed TYPE to ERR_REP (SKL@JACH)
*

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'

        INTEGER LOCO          ! locator to output image
	INTEGER I
	INTEGER J
	INTEGER NDIMS
	INTEGER ODIMS( 2)
        INTEGER NELEMENTS     ! number of elements mapped by NDF_MAP
	INTEGER POS
	INTEGER PNTRO
	INTEGER STATUS
        INTEGER LBND( 2 )     ! lower bounds image array

        DATA LBND / 1, 1 /

	REAL COLOURS( 3, 256)
	REAL INTENSITIES( 3)

	CHARACTER*1 COL_CHAR
	CHARACTER*1 FIRST_COL
	CHARACTER*1 INPUT_CHOICE
	CHARACTER*1 LAST_COL

	LOGICAL MORE

* loops to initialize colour table array to all -1

	DO I = 1, 3
	  DO J = 1, 256
	    COLOURS( I, J) = -1.0E20
	  END DO
	END DO

* get the users choice of colour input types

	CALL PAR_GET0C( 'INPUT_CHOICE', INPUT_CHOICE, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP( 'INPUT',
     :                  'Error after par_get0c input choice ...',
     :                   STATUS )
	  return
	end if

* get the first pen colour

	IF( INPUT_CHOICE .EQ. 'I') THEN

	  CALL MSG_OUT( 'MESSAGE', 'Choose INTENSITIES of PEN number 1',
     *	                STATUS)

	  CALL PAR_GET0R( 'INT1', INTENSITIES( 1), STATUS)
	  CALL PAR_DEF0R( 'INT2', INTENSITIES( 1), STATUS)
	  CALL PAR_DEF0R( 'INT3', INTENSITIES( 1), STATUS)
	  CALL PAR_GET0R( 'INT2', INTENSITIES( 2), STATUS)
	  CALL PAR_GET0R( 'INT3', INTENSITIES( 3), STATUS)

	  CALL PAR_CANCL( 'INT1', STATUS)
	  CALL PAR_CANCL( 'INT2', STATUS)
	  CALL PAR_CANCL( 'INT3', STATUS)
	  if( status .ne. sai__ok) then
	    print *,'after par_get0r intensities ...'
	    return
	  end if

	  CALL MSG_OUT( 'BLANK', ' ', STATUS)

	ELSE

	  CALL PAR_GET0C( 'FIRST_PEN', FIRST_COL, STATUS)
	  if( status .ne. sai__ok) then
	    print *,'after par_get0c first pen ...'
	    return
	  end if

* interpret the intensities of the three guns for the chosen colour

	  CALL GET_INTENSITIES( FIRST_COL, INTENSITIES)

	END IF

* set the colour array intensities for that pen

	COLOURS( 1, 1) = INTENSITIES( 1)
	COLOURS( 2, 1) = INTENSITIES( 2)
	COLOURS( 3, 1) = INTENSITIES( 3)

* get the last pen colour

	IF( INPUT_CHOICE .EQ. 'I') THEN

	  CALL MSG_OUT( 'MESSAGE', 'Choose INTENSITIES of PEN number 256',
     *	                STATUS)

	  CALL PAR_GET0R( 'INT1', INTENSITIES( 1), STATUS)
	  CALL PAR_DEF0R( 'INT2', INTENSITIES( 1), STATUS)
	  CALL PAR_DEF0R( 'INT3', INTENSITIES( 1), STATUS)
	  CALL PAR_GET0R( 'INT2', INTENSITIES( 2), STATUS)
	  CALL PAR_GET0R( 'INT3', INTENSITIES( 3), STATUS)

	  CALL PAR_CANCL( 'INT1', STATUS)
	  CALL PAR_CANCL( 'INT2', STATUS)
	  CALL PAR_CANCL( 'INT3', STATUS)
	  if( status .ne. sai__ok) then
	    print *,'after par_get0r intensities ...'
	    return
	  end if

	  CALL MSG_OUT( 'BLANK', ' ', STATUS)

	ELSE

	  CALL PAR_GET0C( 'LAST_PEN', LAST_COL, STATUS)
	  if( status .ne. sai__ok) then
	    print *,'after par_get0c last pen ...'
	    return
	  end if

* interpret the intensities of the three guns for the chosen colour

	  CALL GET_INTENSITIES( LAST_COL, INTENSITIES)

	END IF

* set the colour array intensities for that pen

	COLOURS( 1, 250) = INTENSITIES( 1)
	COLOURS( 2, 250) = INTENSITIES( 2)
	COLOURS( 3, 250) = INTENSITIES( 3)

	COLOURS( 1, 256) = INTENSITIES( 1)
	COLOURS( 2, 256) = INTENSITIES( 2)
	COLOURS( 3, 256) = INTENSITIES( 3)

* loop to get all user input colours and positions in the colour table

	MORE = .TRUE.

	DO WHILE ( MORE)

* get the position of the colour to be specified

	  CALL PAR_GET0I( 'PEN', POS, STATUS)

	  CALL PAR_CANCL( 'PEN', STATUS)
	  if( status .ne. sai__ok) then
	    print *,'after par_get0i pen position ...'
	    return
	  end if

* test if the position is within the specified limits 0-255

	  IF( POS .LT. 1 .OR. POS .GT. 250) THEN

* user has chosen to end input of colours

	    MORE = .FALSE.

	  ELSE

* continue looping for another colour/pen

	    MORE = .TRUE.

* get the colour that the selected pen is to be set too

	    IF( INPUT_CHOICE .EQ. 'I') THEN

	      CALL MSG_SETI( 'PENNUM', POS)

	      CALL MSG_OUT( 'MESSAGE',
     *	                    'Choose INTENSITIES of PEN number ^PENNUM',
     *	                    STATUS)

	      CALL PAR_GET0R( 'INT1', INTENSITIES( 1), STATUS)
	      CALL PAR_DEF0R( 'INT2', INTENSITIES( 1), STATUS)
	      CALL PAR_DEF0R( 'INT3', INTENSITIES( 1), STATUS)
	      CALL PAR_GET0R( 'INT2', INTENSITIES( 2), STATUS)
	      CALL PAR_GET0R( 'INT3', INTENSITIES( 3), STATUS)

	      CALL PAR_CANCL( 'INT1', STATUS)
	      CALL PAR_CANCL( 'INT2', STATUS)
	      CALL PAR_CANCL( 'INT3', STATUS)
	      if( status .ne. sai__ok) then
	        print *,'after par_get0r intensities ...'
	        return
	      end if

	      CALL MSG_OUT( 'BLANK', ' ', STATUS)

	    ELSE

	      CALL PAR_GET0C( 'COLOUR', COL_CHAR, STATUS)

* cancel pararameter

	      CALL PAR_CANCL( 'COLOUR', STATUS)
	      if( status .ne. sai__ok) then
	        print *,'after par_get0c colour ...'
	        return
	      end if

* interpret the intensities of the three guns for the chosen colour

	      CALL GET_INTENSITIES( COL_CHAR, INTENSITIES)

	    END IF

* set the colour array intensities for that pen

	    COLOURS( 1, POS) = INTENSITIES( 1)
	    COLOURS( 2, POS) = INTENSITIES( 2)
	    COLOURS( 3, POS) = INTENSITIES( 3)

	  END IF

	END DO

* if no more colours are to be specified then interpolate the colours of the
* pens between the chosen pens

	CALL INTERPOLATE_INTENSITIES( COLOURS)

* initialize the size variables for the output HDS container file

	NDIMS=2
	ODIMS(1)=3
	ODIMS(2)=256

* create an HDS file for the colour table

	CALL NDF_CREAT( 'OUTPIC', '_REAL', NDIMS, LBND, ODIMS,
     :                   LOCO, STATUS)


* map the HDS component data_array

        CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                 PNTRO, NELEMENTS, STATUS )

* fill the HDS component data_array with the colour table

	CALL COLTAB_FILL( COLOURS, %VAL(PNTRO))

* release the HDS file

	CALL NDF_ANNUL( LOCO, STATUS )

	CALL PAR_CANCL( 'OUTPIC', STATUS)

	END
