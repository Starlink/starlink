      SUBROUTINE KPG1_CSUBB( BAD, EL, A, CONST, B, NERR, STATUS )
*+
*  Name:
*     KPG1_CSUBx
 
*  Purpose:
*     Subtract a constant from each element of a vectorised array.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_CSUBx( BAD, EL, A, CONST, B, NERR, STATUS )
 
*  Description:
*     The routine subtracts a constant from each element of a
*     vectorised array to produce a new array. Bad value checking is
*     performed if required.
 
*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether to check for bad values in the input array.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     A( EL ) = ? (Given)
*        Input array.
*     CONST = DOUBLE PRECISION (Given)
*        Constant to be subtracted from each array element.
*     B( EL ) = ? (Returned)
*        Output array.
*     NERR = INTEGER (Returned)
*        Number of numerical errors which occurred while processing the
*        array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B or UB as appropriate. The
*     arrays supplied to the routine must have the data type specified.
*     -  This routine will handle numerical errors (i.e. overflow) by
*     assigning the appropriate "bad" value to affected output array
*     elements. If the constant supplied cannot be converted to the
*     data type of the arrays without overflow, then all elements of
*     the output array will be assigned this bad value and NERR will
*     return the value of EL.
 
*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     12-APR-1990 (RFWS):
*        Original version.
*     1996 May 20 (MJC):
*        Replaced LIB$ESTABLISH and LIB$REVERT calls.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
 
*  Global Variables:
      INCLUDE 'NUM_CMN'          ! Numerical error flag
 
*  Arguments Given:
      LOGICAL BAD
      INTEGER EL
      BYTE A( EL )
      DOUBLE PRECISION CONST
 
*  Arguments Returned:
      BYTE B( EL )
      INTEGER NERR
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  External References:
      EXTERNAL NUM_TRAP
      INTEGER NUM_TRAP           ! Numerical error handler
      BYTE VAL_DTOB          ! Convert from double precision
 
*  Local Variables:
      BYTE BCON              ! Type converted constant
      INTEGER I                  ! Loop counter for array elements
      LOGICAL ALLBAD             ! Set output array all bad?
 
*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEC_B'      ! NUM_ arithmetic/maths functions
      INCLUDE 'NUM_DEF_CVT'      ! Define functions...
      INCLUDE 'NUM_DEF_B'
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Open an error context.
      CALL ERR_MARK
 
*  Convert the constant to be subtracted to the appropriate data type.
      ALLBAD = .FALSE.
      BCON = VAL_DTOB( .FALSE., CONST, STATUS )
 
*  If conversion failed, then annul the error and note that all the
*  output array elements must be set to the bad value.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         ALLBAD = .TRUE.
      END IF
 
*  End the error context.
      CALL ERR_RLSE
 
*  Set all output elements to the bad value if required.
      IF ( ALLBAD ) THEN
         DO 1 I = 1, EL
            B( I ) = VAL__BADB
1        CONTINUE
         NERR = EL
 
*  Otherwise establish the numerical error handler and initialise the
*  error flag and error count.
      ELSE
         CALL NUM_HANDL( NUM_TRAP )
         NUM_ERROR = SAI__OK
         NERR = 0
 
*  No bad values present:
*  =====================
         IF ( .NOT. BAD ) THEN
 
*  Subtract the constant from the array.
            DO 2 I = 1, EL
               B( I ) = NUM_SUBB( A( I ), BCON )
 
*  Check for numerical errors (i.e. overflow). If present, then assign
*  a bad value to the output array element and count the error. Reset
*  the numerical error flag.
               IF ( NUM_ERROR .NE. SAI__OK ) THEN
                  B( I ) = VAL__BADB
                  NERR = NERR + 1
                  NUM_ERROR = SAI__OK
               END IF
2           CONTINUE
 
*  Bad values present:
*  ==================
         ELSE
 
*  If the input array element is bad, then so is the output element.
            DO 3 I = 1, EL
               IF ( A( I ) .EQ. VAL__BADB ) THEN
                  B( I ) = VAL__BADB
 
*  Otherwise, subtract the constant, again checking for numerical
*  errors.
               ELSE
                  B( I ) = NUM_SUBB( A( I ), BCON )
                  IF ( NUM_ERROR .NE. SAI__OK ) THEN
                     B( I ) = VAL__BADB
                     NERR = NERR + 1
                     NUM_ERROR = SAI__OK
                  END IF
               END IF
3           CONTINUE
         END IF
 
*  Remove the numerical error handler.
         CALL NUM_REVRT
      END IF
 
      END
