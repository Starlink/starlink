      SUBROUTINE KPG1_NOISUW( BAD, EL, VARNCE, ARRAY, STATUS )
*+
*  Name:
*     KPG1_NOISx

*  Purpose:
*     Adds random Normal noise to a 1-d array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_NOISx( BAD, EL, VARNCE, ARRAY, STATUS )

*  Description:
*     This routine takes a 1-d array and adds noise randomly to each
*     element in the array.  The random number at each element is drawn
*     from a Normal distribution whose spread is given by the
*     corresponding variance array at that element.

*  Arguments:
*     BAD = INTEGER (Given)
*        If true there may be bad pixels in input arrays, and so there
*        will be bad-pixel testing.
*     EL = INTEGER (Given)
*        The number of elements in the arrays.
*     VARNCE( EL ) = ? (Given)
*        The Normal variance array corresponding to the data array.
*     ARRAY( EL ) = ? (Given and Returned)
*        The data array to which random errors are to be applied.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  There is a routine for all numeric data types: replace "x" in
*     the routine name by B, D, I, R, UB, UW or W as appropriate.  The
*     arrays supplied to the routine must have the data type specified.
*     -  All arithmetic is performed in double precision.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 April 25 (MJC):
*        Original version.
*     1997 January 10 (MJC):
*        Replaced NAG calls.  Used commenting modern style.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      LOGICAL BAD
      INTEGER EL
      INTEGER*2 VARNCE( EL )

*  Arguments Given and Returned:
      INTEGER*2 ARRAY( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PDA_RNNOR
      REAL PDA_RNNOR             ! PDA random-number generator with
                                 ! Normal distribution for one value

*  Local Variables:
      INTEGER I                  ! Loop counter
      INTEGER SEED               ! Random-number seed
      REAL SIGMA                 ! Standard deviation
      INTEGER TICKS              ! Clock ticks to randomize initial seed

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the random number generator seed to a non-repeatable
*  value.
      CALL PSX_TIME( TICKS, STATUS )
      SEED = ( TICKS / 4 ) * 4 + 1
      CALL PDA_RNSED( SEED )

*  There are two processing loops depending on whether there might be
*  bad values present.
      IF ( BAD ) THEN

         DO I = 1, EL

*  Check for a bad data-array value or a bad data value.  In this case
*  noise cannot be added to the data array, so set it bad.
            IF ( ARRAY( I ) .EQ. VAL__BADUW .OR.
     :           VARNCE( I ) .EQ. VAL__BADUW ) THEN
               ARRAY( I ) = VAL__BADUW

            ELSE

*  Find the  of the Normal distribution at the element.
               SIGMA = SQRT( NUM_UWTOR( VARNCE( I ) ) )

*  Generate the random noise.  There is no offset.
               ARRAY( I ) = ARRAY( I ) +
     :                      NUM_RTOUW( PDA_RNNOR( 0.0, SIGMA ) )
            END IF
         END DO

      ELSE

         DO  I = 1, EL

*  Find the sigma of the Normal distribution at the element.
            SIGMA = SQRT( NUM_UWTOR( VARNCE( I ) ) )

*  Generate the random noise.  There is no offset.
            ARRAY( I ) = ARRAY( I ) +
     :                   NUM_RTOUW( PDA_RNNOR( 0.0, SIGMA ) )
         END DO
      END IF

      END
