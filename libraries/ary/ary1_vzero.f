      SUBROUTINE ARY1_VZERO( TYPE, N, PNTR, STATUS )
*+
*  Name:
*     ARY1_VZERO

*  Purpose:
*     Set all elements of a vectorised array to zero.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_VZERO( TYPE, N, PNTR, STATUS )

*  Description:
*     The routine sets all elements of a vectorised array, of any
*     numeric data type, to zero. The array is passed by pointer.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        An HDS primitive numeric type string specifying the data type
*        of the array (case insensitive).
*     N = INTEGER (Given)
*        Number of array elements to be set to zero.
*     PNTR = INTEGER (Given)
*        Pointer to the array whose elements are to be set to zero (the
*        pointer value itself is not changed, although the array
*        elements are).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  According to the data type specified, call an appropriate
*     routine to set all the array elements to zero.
*     -  If the data type string was not valid, then report an error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JUN-1989  (RFWS):
*        Original version.
*     30-AUG-1989 (RFWS):
*        Changed calls to renamed VEC_ZERx routines.
*     7-SEP-1989 (RFWS):
*        Added message token for routine name, to prevent '$' from
*        affecting error messages.
*     13-MAR-1990 (RFWS):
*        Changed to call ARY1_ZERx instead of VEC_ZERx.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNV_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER N
      INTEGER PNTR

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Test the data type against each valid value in turn, calling the
*  appropriate routine to set the array elements to zero.
      IF ( CHR_SIMLR( TYPE, '_BYTE' ) ) THEN
         CALL ARY1_ZERB( N, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_UBYTE' ) ) THEN
         CALL ARY1_ZERUB( N, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_DOUBLE' ) ) THEN
         CALL ARY1_ZERD( N, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_INTEGER' ) ) THEN
         CALL ARY1_ZERI( N, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_REAL' ) ) THEN
         CALL ARY1_ZERR( N, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_WORD' ) ) THEN
         CALL ARY1_ZERW( N, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_UWORD' ) ) THEN
         CALL ARY1_ZERUW( N, %VAL( CNF_PVAL( PNTR ) ), STATUS )

*  If the data type supplied was not valid, then report an error.
      ELSE
         STATUS = ARY__FATIN
         CALL MSG_SETC( 'ROUTINE', 'ARY1_VZERO' )
         CALL MSG_SETC( 'BADTYPE', TYPE )
         CALL ERR_REP( 'ARY1_VZERO_TYPE',
     :   'Routine ^ROUTINE called with an invalid TYPE argument ' //
     :   'of ''^BADTYPE'' (internal programming error).', STATUS )
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_VZERO', STATUS )

      END
