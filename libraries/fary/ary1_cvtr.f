      SUBROUTINE ARY1_CVTR( BAD, N, TYPE, PNTR, RESULT, DCE, STATUS )
*+
*  Name:
*     ARY1_CVTR
 
*  Purpose:
*     Convert a vectorised array to REAL.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL ARY1_CVTR( BAD, N, TYPE, PNTR, RESULT, DCE, STATUS )
 
*  Description:
*     The routine converts a vectorised array of any primitive numeric
*     type to a data type of REAL, checking for the presence of bad
*     pixels if required. The input array is passed by pointer; the
*     output array is passed directly.
 
*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether to check for bad pixel values.
*     N = INTEGER (Given)
*        Number of array elements to convert.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the input vectorised array. This value must be
*        one of the HDS primitive numeric data type strings and must be
*        supplied in upper case.
*     PNTR = INTEGER (Given)
*        Pointer to the input vectorised array.
*     RESULT( N ) = REAL (Returned)
*        The output REAL vectorised array, to contain the converted
*        values.
*     DCE = LOGICAL (Returned)
*        Whether any data conversion errors occurred (bad values are
*        entered into the affected elements of the output array if this
*        happens).
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Algorithm:
*     -  Initialise and mark the error stack.
*     -  Test the input data type specification against each valid value
*     in turn, calling the appropriate data conversion routine.
*     -  Note if the data type specification is not valid.
*     -  If a data conversion error ocurred, note this fact and annul
*     any error reports this might have generated.
*     -  Release the error stack.
*     -  If the input data type specification was not valid, then report
*     an error.
 
*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     13-JUN-1989 (RFWS):
*        Original version.
*     30-AUG-1989 (RFWS):
*        Changed to use message token to prevent '$' affecting error
*        messages.
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
      LOGICAL BAD
      INTEGER N
      CHARACTER * ( * ) TYPE
      INTEGER PNTR
 
*  Arguments Returned:
      REAL RESULT
      LOGICAL DCE
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local variables:
      INTEGER IERR               ! Position of first conversion error
      INTEGER NERR               ! Number of conversion errors
      LOGICAL TYPOK              ! Whether the TYPE argument is valid
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Initialise and mark the error stack.
      TYPOK = .TRUE.
      NERR = 0
      CALL ERR_MARK
 
*  Test for each valid input data type in turn and call the appropriate
*  conversion routine.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL VEC_BTOR( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT, IERR,
     :                  NERR, STATUS )
 
      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL VEC_UBTOR( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT, IERR,
     :                   NERR, STATUS )
 
      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL VEC_DTOR( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT, IERR,
     :                  NERR, STATUS )
 
      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL VEC_ITOR( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT, IERR,
     :                  NERR, STATUS )
 
      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL VEC_RTOR( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT, IERR,
     :                  NERR, STATUS )
 
      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL VEC_WTOR( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT, IERR,
     :                  NERR, STATUS )
 
      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL VEC_UWTOR( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT, IERR,
     :                   NERR, STATUS )
 
*  Note if the input data type specified is not valid.
      ELSE
         TYPOK = .FALSE.
      END IF
 
*  If a data conversion error occurred, then annul any error reports it
*  might have produced.
      DCE = NERR .NE. 0
      IF ( DCE ) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE
 
*  Report an error if the input data type specified was not valid.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF( .NOT. TYPOK ) THEN
            STATUS = ARY__FATIN
            CALL MSG_SETC( 'ROUTINE', 'ARY1_CVTR' )
            CALL MSG_SETC( 'BADTYPE', TYPE )
            CALL ERR_REP( 'ARY1_CVTR_TYPE',
     :      'Routine ^ROUTINE called with an invalid TYPE ' //
     :      'argument of ''^BADTYPE'' (internal programming error).',
     :      STATUS )
         END IF
      END IF
 
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_CVTR',
     :STATUS )
 
      END
