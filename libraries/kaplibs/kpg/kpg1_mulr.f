      SUBROUTINE KPG1_MULR( BAD, VAR, EL, A, VA, B, VB, C, VC, NERR,
     :                        STATUS )
*+
*  Name:
*     KPG1_MULx
 
*  Purpose:
*     Multiply two vectorised arrays with optional variance information.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_MULx( BAD, VAR, EL, A, VA, B, VB, C, VC, NERR, STATUS )
 
*  Description:
*     The routine forms the product of two vectorised arrays, with
*     optional variance information. Bad value checking is also
*     performed if required.
 
*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for bad values in the input
*        arrays. The routine will execute more rapidly if this checking
*        can be omitted.
*     VAR = LOGICAL (Given)
*        Whether associated variance information is to be processed.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     A( EL ) = ? (Given)
*        First array of data to be multiplied.
*     VA( EL ) = ? (Given)
*        Variance values associated with the array A. Not used if VAR
*        is set to .FALSE..
*     B( EL ) = ? (Given)
*        Second array of data to be multipled.
*     VB( EL ) = ? (Given)
*        Variance values associated with the array B. Not used if VAR
*        is set to .FALSE..
*     C( EL ) = ? (Returned)
*        Result of multiplying arrays A and B.
*     VC( EL ) = ? (Returned)
*        Variance values associated with the array C. Not used if VAR
*        is set to .FALSE..
*     NERR = INTEGER (Returned)
*        Number of numerical errors which occurred during the
*        calculations.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for each of the data types integer, real
*     and double precision: replace "x" in the routine name by I, R or
*     D as appropriate. The arrays passed to this routine should all
*     have the specified data type.
*     -  This routine will handle numerical overflow. If overflow
*     occurs, then affected output array elements will be set to the
*     "bad" value. A count of the numerical errors which occur is
*     returned via the NERR argument.
 
*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     9-APR-1990 (RFWS):
*        Original version.
*     1991 February 26 (MJC):
*        Fixed bug when VEC routine returns bad status.
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
      INCLUDE 'NUM_CMN'          ! Define NUM_ERROR status flag
 
*  Arguments Given:
      LOGICAL BAD
      LOGICAL VAR
      INTEGER EL
      REAL A( EL )
      REAL VA( EL )
      REAL B( EL )
      REAL VB( EL )
 
*  Arguments Returned:
      REAL C( EL )
      REAL VC( EL )
      INTEGER NERR
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  External References:
      EXTERNAL NUM_TRAP
      INTEGER NUM_TRAP           ! Numerical error handler
 
*  Local Variables:
      INTEGER I                  ! Loop counter for array elements
      INTEGER IERR               ! Initial error position (dummy)
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  No variance component to process:
*  ================================
*  Simply multiply the two data arrays.
      IF ( .NOT. VAR ) THEN
         CALL ERR_MARK
         CALL VEC_MULR( BAD, EL, A, B, C, IERR, NERR, STATUS )
 
*  Annul the bad status due to any overflows.
         CALL ERR_ANNUL( STATUS )
         CALL ERR_RLSE
 
*  Variance component present:
*  ==========================
*  Establish a numerical error handler and initialise the numerical
*  error status and error count.
      ELSE
         CALL NUM_HANDL( NUM_TRAP )
         NUM_ERROR = SAI__OK
         NERR = 0
 
*  No bad values present:
*  =====================
         IF ( .NOT. BAD ) THEN
 
*  Multiply the data arrays, checking for numerical overflow after each
*  calculation.
            DO 1 I = 1, EL
               C( I ) = A( I ) * B( I )
               IF ( NUM_ERROR .NE. SAI__OK ) THEN
                  C( I ) = VAL__BADR
                  NERR = NERR + 1
                  NUM_ERROR = SAI__OK
               END IF
 
*  Derive the variance values, again checking for overflow.
               VC( I ) = VA( I ) * B( I ) * B( I ) +
     :                   VB( I ) * A( I ) * A( I )
               IF ( NUM_ERROR .NE. SAI__OK ) THEN
                  VC( I ) = VAL__BADR
                  NERR = NERR + 1
                  NUM_ERROR = SAI__OK
               END IF
1           CONTINUE
 
*  Bad values present:
*  ==================
         ELSE
            DO 2 I = 1, EL
 
*  See if either input data value is bad. If so, then set bad output
*  values.
               IF ( ( A( I ) .EQ. VAL__BADR ) .OR.
     :              ( B( I ) .EQ. VAL__BADR ) ) THEN
                  C( I ) = VAL__BADR
                  VC( I ) = VAL__BADR
 
*  Multiply the data values, checking for numerical overflow.
               ELSE
                  C( I ) = A( I ) * B( I )
                  IF ( NUM_ERROR .NE. SAI__OK ) THEN
                     C( I ) = VAL__BADR
                     NERR = NERR + 1
                     NUM_ERROR = SAI__OK
                  END IF
 
*  See if either input variance value is bad. If so, then set a bad
*  output variance value.
                  IF ( ( VA( I ) .EQ. VAL__BADR ) .OR.
     :                 ( VB( I ) .EQ. VAL__BADR ) ) THEN
                     VC( I ) = VAL__BADR
 
*  Calculate the output variance value, again checking for overflow.
                  ELSE
                     VC( I ) = VA( I ) * B( I ) * B( I ) +
     :                         VB( I ) * A( I ) * A( I )
                     IF ( NUM_ERROR .NE. SAI__OK ) THEN
                        VC( I ) = VAL__BADR
                        NERR = NERR + 1
                        NUM_ERROR = SAI__OK
                     END IF
                  END IF
               END IF
2           CONTINUE
         END IF
 
*  Remove the numerical error handler.
         CALL NUM_REVRT
      END IF
 
      END
