      SUBROUTINE KPG1_CMULUB( BAD, EL, A, CONST, B, NBAD, STATUS )
*+
*  Name:
*     KPG1_CMULx
 
*  Purpose:
*     Multiply each element of a vectorised array by a constant.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_CMULx( BAD, EL, A, CONST, B, NBAD, STATUS )
 
*  Description:
*     The routine multiplies each element of a vectorised array by a
*     constant to produce a new array. Bad value checking is performed
*     if required.
 
*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether to check for bad values in the input array.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     A( EL ) = ? (Given)
*        Input array.
*     CONST = DOUBLE PRECISION (Given)
*        Constant by which each array element is to be multiplied.
*     B( EL ) = ? (Returned)
*        Output array.
*     NBAD = INTEGER (Returned)
*        Number of bad values in the output array B.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for each numeric type except double
*     precision: replace "x" in the routine name by R, I, W, UW, B or
*     UB as appropriate. The arrays supplied to the routine must have
*     the data type specified.
*     -  This routine will handle numerical errors (i.e. overflow) by
*     assigning the appropriate "bad" value to affected output array
*     elements.
 
*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     17-APR-1990 (RFWS):
*        Original version.
*     11-MAR-1991 (RFWS):
*        Changed to return the total number of bad pixels in the output
*        array.
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
      INTEGER NBAD
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  External References:
      EXTERNAL NUM_TRAP
      INTEGER NUM_TRAP           ! Numerical error handler
 
*  Local Variables:
      INTEGER I                  ! Loop counter for array elements
      REAL RCONST                ! Single precision constsnt value
 
*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEF_CVT'      ! Define functions...
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Obtain a single-precision version of the multiplication constant.
      RCONST = SNGL( CONST )
 
*  Establish a numerical error handler and initialise the error flag
*  and error count.
      CALL NUM_HANDL( NUM_TRAP )
      NUM_ERROR = SAI__OK
      NBAD = 0
 
*  No bad values present:
*  =====================
      IF ( .NOT. BAD ) THEN
 
*  Multiply the array by the constant, using floating-point arithmetic.
         DO 1 I = 1, EL
            B( I ) = NUM_RTOUB( RCONST * NUM_UBTOR( A( I ) ) )
 
*  Check for numerical errors (i.e. overflow). If present, then assign
*  a bad value to the output array element and count the error. Reset
*  the numerical error flag.
            IF ( NUM_ERROR .NE. SAI__OK ) THEN
               B( I ) = VAL__BADUB
               NBAD = NBAD + 1
               NUM_ERROR = SAI__OK
            END IF
 1       CONTINUE
 
*  Bad values present:
*  ==================
      ELSE
 
*  If the input array element is bad, then so is the output element.
         DO 2 I = 1, EL
            IF ( A( I ) .EQ. VAL__BADUB ) THEN
               B( I ) = VAL__BADUB
               NBAD = NBAD + 1
 
*  Otherwise, multiply by the constant, again checking for numerical
*  errors.
            ELSE
               B( I ) = NUM_RTOUB( RCONST * NUM_UBTOR( A( I ) ) )
               IF ( NUM_ERROR .NE. SAI__OK ) THEN
                  B( I ) = VAL__BADUB
                  NBAD = NBAD + 1
                  NUM_ERROR = SAI__OK
               END IF
            END IF
 2       CONTINUE
      END IF
 
*  Remove the numerical error handler.
      CALL NUM_REVRT
 
      END
