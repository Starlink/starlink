************************************************************************

	INTEGER FUNCTION CYC_DAT( CYCLE )

*+
*  Name :
*     CYC_DAT
*
*  Purpose :
*     {routine_purpose}...
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL CYC_DAT( CYCLE )
*
*  Description :
*     {routine_description}...
*
*  Arguments :
*     {arguement_description}...
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     KM: Koji Mukai (Oxford University)
*     AA: Alasdair Allan (Starlink, Keele University)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}
*
*  History :
*     14-FEB-1999
*        Cut and hack for Starlink
*     21-FEB-2008 (PWD):
*        Stop using internal writes to copy constant strings. Initialise
*        STATUS so that messages are output.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	REAL CYCLE

	LOGICAL CYCLIC_DATA
	REAL CYCLE_DATA
	COMMON / DATA_CYCLIC / CYCLIC_DATA, CYCLE_DATA
        CHARACTER TEXT * 72

	INTEGER STATUS

	STATUS = SAI__OK

	IF( CYCLE .LE. 0.0 ) THEN
	  TEXT = 'ERROR > CYC_DAT unacceptable periodicity'
	  CALL MSG_OUT(' ', TEXT, STATUS)
	  CYCLIC_DATA = .FALSE.
	  CYCLE_DATA = 0.0
	  CYC_DAT = -1
	ELSE
	  CYCLIC_DATA = .TRUE.
	  CYCLE_DATA = CYCLE
	  CYC_DAT = 1
	END IF

	END
