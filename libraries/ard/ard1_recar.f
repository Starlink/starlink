      SUBROUTINE ARD1_RECAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND,
     :                       NARG, I, KEYW, STATUS )
*+
*  Name:
*     ARD1_RECAR

*  Purpose:
*     Assemble argument list for a RECT keyword

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_RECAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND, NARG,
*                      I, KEYW, STATUS )

*  Description:
*     The co-efficients of the current transformation from user
*     co-ordinates to pixel co-ordinates are stored on the stack,
*     followed by the supplied user co-ordinates of the two diagonally
*     opposite corners.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The dimensionality of the ARD description (i.e. the number of
*        values required to specify a position).
*     C( * ) = REAL (Given)
*        The co-efficients of the current mapping from user
*        co-ordinates to pixel co-ordinates.
*     ELEM = CHARACTER * ( * ) (Given)
*        An element of an ARD description.
*     L = INTEGER (Given)
*        The index of the last character in ELEM to be checked.
*     IPOPND = INTEGER (Given)
*        The pointer to the array holding the operand stack.
*     IOPND = INTEGER (Given and Returned)
*        The index within the operand stack at which the next value
*        should be stored.
*     SZOPND = INTEGER (Given and Returned)
*        The size of the operand stack. This is increased if necessary.
*     NARG = INTEGER (Given and Returned)
*        The number of arguments so far obtained. This should be
*        supplied equal to -1 if no argument list has yet been found.
*     I = INTEGER (Given and Returned)
*        The index of the next character to be checked in ELEM.
*     KEYW = LOGICAL (Given and Returned)
*        Returned as .FALSE. if the argument list for the keyword has
*        been completed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-FEB-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants

*  Arguments Given:
      INTEGER NDIM
      REAL C( * )
      CHARACTER ELEM*(*)
      INTEGER L

*  Arguments Given and Returned:
      INTEGER IPOPND
      INTEGER IOPND
      INTEGER SZOPND
      INTEGER NARG
      INTEGER I
      LOGICAL KEYW

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ID                 ! Loop count
      LOGICAL OK                 ! Was an argument value obtained?
      REAL VALUE                 ! The argument value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Attempt to read argument value from the current element until the
*  end of the element, or the end of the argument list is encountered.
      DO WHILE( I .LE. L .AND. KEYW .AND. STATUS .EQ. SAI__OK ) 

*  Read the next argument.
         CALL ARD1_GTARG( ELEM, L, I, OK, KEYW, VALUE, STATUS )

*  If an argument was obtained, increment the number of arguments
*  obtained so far.
         IF( OK ) THEN
            NARG = NARG + 1

*  If an argument list has just been started, store the co-efficients
*  of the current transformation on the operand stack.
            IF( NARG .EQ. 1 ) THEN
               DO ID = 1, NDIM*( NDIM + 1 )
                  CALL ARD1_STORR( C( ID ), SZOPND, IOPND, IPOPND,
     :                             STATUS )
               END DO         
            END IF

*  Store the argument value in the operand stack.
            CALL ARD1_STORR( VALUE, SZOPND, IOPND, IPOPND, STATUS )

*  If the end of the argument list has been reached, report an error if
*  the number of arguments obtained is incorrect.
         ELSE IF( .NOT. KEYW ) THEN
 
            IF( NARG .NE. 2*NDIM .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = ARD__ARGS
               CALL ERR_REP( 'ARD1_RECAR_ERR1', 'Incorrect number of '//
     :                       'arguments found.', STATUS )
            END IF

         END IF

      END DO

      END
