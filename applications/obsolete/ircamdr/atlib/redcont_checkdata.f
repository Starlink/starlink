	SUBROUTINE REDCONT_CHECKDATA( CELL_COUNTER, OBSLOC, FILLED, STATUS)

* Description : Look to see if CONFIGURATION primitive is defined and set
*               the FILLED logical variable to indicate it's state.

* ==============================================================================

* Parameters :

* Method :

* Deficiencies :

* Bugs :

* Authors : C.Aspin (ROE), REVA::CAA : 14-JUL-86

* History :
* endhistory

* Type Definitions :

	IMPLICIT NONE

* Global constants :

	INCLUDE 'SAE_PAR'
        INCLUDE 'DAT_PAR'      ! Necessary for non-VMS

* Import :

	INTEGER CELL_COUNTER

	CHARACTER*( *) OBSLOC

* Import-Export :

* Export :

	LOGICAL FILLED

* Status :

	INTEGER STATUS

* External references :

* Global variables :

* Local Constants :

* Local variables :

	INTEGER SUBS( 2)

	CHARACTER*( DAT__SZLOC) OBSCELL_LOCATOR
	CHARACTER*( DAT__SZLOC) TMP_LOCATOR

* Internal References :

* Local data :

* ==============================================================================

* check status on entry

	IF( STATUS .NE. SAI__OK) THEN

	  RETURN

	END IF

* setup the subs dimension array

	SUBS( 1) = CELL_COUNTER
	SUBS( 2) = 0

* get locator for the Nth cell of the OBSERVATIONS structure

	CALL DAT_CELL( OBSLOC, 1, SUBS, OBSCELL_LOCATOR,
     :	               STATUS)

* get locator for CONFIGURATION structure

	CALL DAT_FIND( OBSCELL_LOCATOR, 'CONFIGURATION', TMP_LOCATOR,
     :	               STATUS)

* find out if the CONFIGURATION component has been filled

	CALL DAT_STATE( TMP_LOCATOR, FILLED, STATUS)

* annul all active locator

	CALL DAT_ANNUL( TMP_LOCATOR, STATUS)
	CALL DAT_ANNUL( OBSCELL_LOCATOR, STATUS)

	END
