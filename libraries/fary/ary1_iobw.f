      SUBROUTINE ARY1_IOBW( TYPE, INOPT, EL, PNTR, STATUS )
*+
*  Name:
*     ARY1_IOBW

*  Purpose:
*     Initialise an object for writing.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_IOBW( TYPE, INOPT, EL, PNTR, STATUS )

*  Description:
*     The routine performs initialisation of a vectorised array, of any
*     numeric data type, prior to its use for 'WRITE' access.
*     Initialisation may be to zero, or to "bad" values, according to
*     the value supplied for the INOPT argument. If this argument is a
*     blabk string, then no action is taken. The array is passed by
*     pointer.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        An HDS primitive numeric type string specifying the data type
*        of the object to be initialised (case insensitive).
*     INOPT = CHARACTER * ( * ) (Given)
*        The initialisation option required; either ' ', 'ZERO' or
*        'BAD' (case insensitive).
*     EL = INTEGER (Given)
*        Number of elements in the array.
*     PNTR = INTEGER (Given)
*        Pointer to the array to be initialised (the pointer itself is
*        not altered, although the array elements are).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  If INOPT is ' ', then do nothing.
*     -  If INOPT is 'ZERO', then set all the array elements to zero.
*     -  If INOPT is 'BAD', then set all the array elements to the
*     appropriate "bad" value.
*     -  If INOPT has an invalid value, then report an error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JUN-1989  (RFWS):
*        Original version.
*     16-AUG-1989 (RFWS):
*        Changed to allow a blank string for the INOPT argument,
*        specifying that no action be taken.
*     30-AUG-1989 (RFWS):
*        Changed to use a message token for the routine name to prevent
*        '$' affecting error messages.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) INOPT
      INTEGER EL
      INTEGER PNTR

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do nothing if INOPT is a blank string.
      IF ( INOPT .EQ. ' ' ) THEN
         CONTINUE

*  If requested, set all the array elements to zero.
      ELSE IF ( CHR_SIMLR( INOPT, 'ZERO' ) ) THEN
         CALL ARY1_VZERO( TYPE, EL, PNTR, STATUS )

*  Otherwise, if requested, set all the array elements to a "bad" value.
      ELSE IF ( CHR_SIMLR( INOPT, 'BAD' ) ) THEN
         CALL ARY1_VBAD( TYPE, EL, PNTR, STATUS )

*  If the initialisation option specified was not valid, then report an
*  error.
      ELSE
         STATUS = ARY__FATIN
         CALL MSG_SETC( 'ROUTINE', 'ARY1_IOBW' )
         CALL MSG_SETC( 'BADINOPT', INOPT )
         CALL ERR_REP( 'ARY1_IOBW_INOPT',
     :   'Routine ^ROUTINE called with an invalid INOPT argument ' //
     :   'of ''^BADINOPT'' (internal programming error).', STATUS )
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_IOBW', STATUS )

      END
