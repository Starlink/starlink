      SUBROUTINE CCG1_CMLTR( BAD, EL, A, CONST, B, NERR, STATUS )
*+
*  Name:
*     CCG1_CMLTR

*  Purpose:
*     To multiply each element of a vectorised array by a constant.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCG1_CMLTR( BAD, EL, A, CONST, B, NERR, STATUS )

*  Description:
*     The routine multiplies each element of a vectorised array by a
*     consta to produce a new array. Bad value checking is performed if
*     required.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether to check for bad values in the input array.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     A( EL ) = REAL (Given)
*        Input array.
*     CONST = DOUBLE PRECISION (Given)
*        Constant to multiply each array element by.
*     B( EL ) = REAL (Returned)
*        Output array.
*     NERR = INTEGER (Returned)
*        Number of numerical errors which occurred while processing the
*        array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine will handle numerical errors (i.e. overflow) by
*     assigning the appropriate "bad" value to affected output array
*     elements. If the constant supplied cannot be converted to the
*     data type of the arrays without overflow, then all elements of
*     the output array will be assigned this bad value and NERR will
*     return the value of EL.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     BRADC: Brad Cavanagh (JAC)
*     {enter_new_authors_here}

*  History:
*     30-APR-1991 (PDRAPER):
*        Based on RFWSs KPG1_ routines.
*     11-OCT-2004 (BRADC):
*        No longer use NUM_CMN.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! BAD data constants

*  Global Variables:


*  Arguments Given:
      LOGICAL BAD
      INTEGER EL
      REAL A( EL )
      DOUBLE PRECISION CONST

*  Arguments Returned:
      REAL B( EL )
      INTEGER NERR

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL NUM_WASOK
      LOGICAL NUM_WASOK          ! Was numeric operation ok?
      EXTERNAL NUM_TRAP
      INTEGER NUM_TRAP           ! Numerical error handler
      REAL VAL_DTOR          ! Convert from double precision

*  Local Variables:
      REAL RCON              ! Type converted constant
      INTEGER I                  ! Loop counter for array elements
      LOGICAL ALLBAD             ! Set output array all bad?

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEC_R'      ! NUM_ arithmetic/maths functions
      INCLUDE 'NUM_DEF_CVT'      ! Define functions...
      INCLUDE 'NUM_DEF_R'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open an error context.
      CALL ERR_MARK

*  Convert the constant to be added to the appropriate data type.
      ALLBAD = .FALSE.
      RCON = VAL_DTOR( .FALSE., CONST, STATUS )

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
            B( I ) = VAL__BADR
1        CONTINUE
         NERR = EL

*  Otherwise establish the numerical error handler and initialise the
*  error flag and error count.
      ELSE
         CALL NUM_HANDL( NUM_TRAP )
         CALL NUM_CLEARERR()
         NERR = 0

*  No bad values present:
*  =====================
         IF ( .NOT. BAD ) THEN

*  Multiply the array value by the constant.
            DO 2 I = 1, EL
               B( I ) = NUM_MULR( A( I ), RCON )

*  Check for numerical errors (i.e. overflow). If present, then assign
*  a bad value to the output array element and count the error. Reset
*  the numerical error flag.
               IF ( .NOT. NUM_WASOK() ) THEN
                  B( I ) = VAL__BADR
                  NERR = NERR + 1
                  CALL NUM_CLEARERR()
               END IF
2           CONTINUE

*  Bad values present:
*  ==================
         ELSE

*  If the input array element is bad, then so is the output element.
            DO 3 I = 1, EL
               IF ( A( I ) .EQ. VAL__BADR ) THEN
                  B( I ) = VAL__BADR

*  Otherwise, multiply by the constant, again checking for numerical
*  errors.
               ELSE
                  B( I ) = NUM_MULR( A( I ), RCON )
                  IF ( .NOT. NUM_WASOK() ) THEN
                     B( I ) = VAL__BADR
                     NERR = NERR + 1
                     CALL NUM_CLEARERR()
                  END IF
               END IF
3           CONTINUE
         END IF

*  Remove the numerical error handler.
         CALL NUM_REVRT
      END IF

      END
* @(#)ccg1_cmlt.gen	2.2     10/8/96     2
