      SUBROUTINE ARD1_ROTAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND,
     :                       NARG, I, KEYW, STATUS )
*+
*  Name:
*     ARD1_ROTAR

*  Purpose:
*     Assemble argument list for a ROTBOX keyword

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_ROTAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND, NARG,
*                      I, KEYW, STATUS )

*  Description:
*     The pixel co-ordinates of the four corners of the rotated box are
*     stored on the operand stack. They are in an order such that they
*     can be considered to be a 4-vertex polygon.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The dimensionality of the ARD description (i.e. the number of
*        values required to specify a position).
*     C( * ) = REAL (Given)
*        The co-efficients of the current mapping from supplied
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
      LOGICAL OK                 ! Was an argument value obtained?
      REAL
     : ARGS( ARD__MXDIM ),       ! A set of co-ordinates
     : CORN( 2 ),                ! Corner co-ordinates
     : SINT,                     ! SIN of the rotation angle
     : COST,                     ! COS of the rotation angle
     : VALUE                     ! The argument value

*  Ensure that the ARGS array is saved between invocations of this
*  routine.
      SAVE ARGS
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error and abort if the dimensionality is not 2.
      IF( NDIM .NE. 2 ) THEN
         STATUS = ARD__NOT2D
         CALL ERR_REP( 'ARD1_ROTAR_ERR1', 'ROTBOX keyword found in '//
     :                 'non-2D ARD description.', STATUS )
         GO TO 999
      END IF

*  Attempt to read argument value from the current element until the
*  end of the element, or the end of the argument list is encountered.
      DO WHILE( I .LE. L .AND. KEYW .AND. STATUS .EQ. SAI__OK ) 

*  Read the next argument.
         CALL ARD1_GTARG( ELEM, L, I, OK, KEYW, VALUE, STATUS )

*  If an argument was obtained, increment the number of arguments
*  supplied by the user and store the new argument (so long as the array
*  is not full).
         IF( OK .AND. NARG .LT. 5 ) THEN
            NARG = NARG + 1
            ARGS( NARG ) = VALUE

*  If all the arguments have been supplied...
            IF( NARG .EQ. 5 ) THEN

*  Save commonly used values.
               SINT = SIN( ARGS( 5 ) * ARD__DTOR )
               COST = COS( ARGS( 5 ) * ARD__DTOR )

*  Set up the user coordinates of the first corner of the box.
               CORN( 1 ) = 0.5*( -ARGS( 3 )*COST + ARGS( 4 )*SINT )
     :                     + ARGS( 1 )
               CORN( 2 ) = 0.5*( -ARGS( 4 )*COST - ARGS( 3 )*SINT )
     :                     + ARGS( 2 )

*  Transform the user co-ordinates into pixel co-ordinates, and store
*  them on the operand stack.
               CALL ARD1_STORP( NDIM, C, CORN, IPOPND, IOPND, SZOPND,
     :                          STATUS )

*  Now do the same with the second corner.
               CORN( 1 ) = 0.5*( -ARGS( 3 )*COST - ARGS( 4 )*SINT )
     :                     + ARGS( 1 )
               CORN( 2 ) = 0.5*( ARGS( 4 )*COST - ARGS( 3 )*SINT )
     :                     + ARGS( 2 )
               CALL ARD1_STORP( NDIM, C, CORN, IPOPND, IOPND, SZOPND,
     :                          STATUS )
      
*  Now do the same with the third corner.
               CORN( 1 ) = 0.5*( ARGS( 3 )*COST - ARGS( 4 )*SINT )
     :                     + ARGS( 1 )
               CORN( 2 ) = 0.5*( ARGS( 4 )*COST + ARGS( 3 )*SINT )
     :                     + ARGS( 2 )
               CALL ARD1_STORP( NDIM, C, CORN, IPOPND, IOPND, SZOPND,
     :                          STATUS )

*  Now do the same with the fourth corner.
               CORN( 1 ) = 0.5*( ARGS( 3 )*COST + ARGS( 4 )*SINT )
     :                     + ARGS( 1 )
               CORN( 2 ) = 0.5*( -ARGS( 4 )*COST + ARGS( 3 )*SINT )
     :                     + ARGS( 2 )
               CALL ARD1_STORP( NDIM, C, CORN, IPOPND, IOPND, SZOPND,
     :                          STATUS )

            END IF

*  If the end of the argument list has been reached, report an error if
*  the number of arguments obtained is incorrect.
         ELSE IF( .NOT. KEYW ) THEN
 
            IF( NARG .NE. 5 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = ARD__ARGS
               CALL ERR_REP( 'ARD1_ROTAR_ERR', 'Incorrect number of '//
     :                       'arguments found.', STATUS )
            END IF

         END IF

      END DO

*  Jump to here if an error occurs.
 999  CONTINUE

      END
