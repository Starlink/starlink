	SUBROUTINE REDCONT_WHATSET( CELL_COUNTER, OBSLOC, WHAT_FILLED,
     :	                            STATUS)
C
C Description : Test what DATA_ARRAY primitives are set by looking at values
C
C ==============================================================================
C
C Parameters :
C
C Method :
C
C Deficiencies :
C
C Bugs :
C
C Authors : C.Aspin (ROE), REVA::CAA : 14-JUL-86
C
C History :
C    14-JUL-1994 Changed MSG_OUT on error to ERR_REP (SKL@JACH)
C endhistory
C
C Type Definitions :
C
	IMPLICIT NONE
C
C Global constants :
C
	INCLUDE 'SAE_PAR'
        INCLUDE 'DAT_PAR'      ! Necessary for non-VMS
C
C Import :
C
	INTEGER CELL_COUNTER

	CHARACTER*( *) OBSLOC
C
C Import-Export :
C
C Export :
C
	CHARACTER*( *) WHAT_FILLED
C
C Status :
C
	INTEGER STATUS
C
C External references :
C
C Global variables :
C
C Local Constants :
C
C Local variables :
C
	INTEGER DIMS( 2)
	INTEGER SUBS( 2)

	CHARACTER*20 CONFIG
	CHARACTER*20 MODE

	CHARACTER*( DAT__SZLOC) OBSCELL_LOCATOR

C
C Internal References :
C
C Local data :
C
C ==============================================================================
C
C check status on entry
C
	IF( STATUS .NE. SAI__OK) THEN

	  RETURN

	END IF
C
C initializes the dimension array
C
	DIMS( 1) = 0
	DIMS( 2) = 0
C
C setup the subs dimension array
C
	SUBS( 1) = CELL_COUNTER
	SUBS( 2) = 0
C
C get locator for the Nth cell of the OBSERVATIONS structure
C
	CALL DAT_CELL( OBSLOC, 1, SUBS, OBSCELL_LOCATOR, STATUS)
C
C find the mode component and read value
C
	CALL CMP_GET0C( OBSCELL_LOCATOR, 'MODE', MODE, STATUS)
C
C find the configuration component and read value
C
	CALL CMP_GET0C( OBSCELL_LOCATOR, 'CONFIGURATION', CONFIG, STATUS)

	IF( STATUS .NE. SAI__OK) THEN

	  CALL ERR_REP( 'ERROR', 'after CMP_GET0C MODE,CONFIG ...',
     :                   STATUS)
	  RETURN

	END IF
C
C test the values of the mode and configuration for what data is set
C
	IF( CONFIG .EQ. 'STARE' .AND. MODE .EQ. 'KTC OFF') THEN

	  WHAT_FILLED = 'PHASEA'

	ELSE IF( CONFIG .EQ. 'STARE' .AND. MODE .EQ. 'KTC ON') THEN

	  WHAT_FILLED = 'PHASEA+KTCA'

	ELSE IF( CONFIG .EQ. 'CHOP' .AND. MODE .EQ. 'KTC OFF') THEN

	  WHAT_FILLED = 'PHASEA+PHASEB'

	ELSE IF( CONFIG .EQ. 'CHOP' .AND. MODE .EQ. 'KTC ON') THEN

	  WHAT_FILLED = 'PHASEA+PHASEB+KTCA+KTCB'

	ELSE

	  CALL MSG_OUT( 'ERROR', 'Error, bad CONFIGURE/MODE values',
     :	    STATUS)

	  WHAT_FILLED = 'NO IDEA'

	END IF

	END
