      SUBROUTINE FIT1_NDOF( NGOOD, MODEL, FROZEN, NDOF, STATUS )
*+
*  Name:
*     FIT1_NDOF

*  Purpose:
*     Determine number of degrees of freedom for chi-squared fitting

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIT1_NDOF( NGOOD, MODEL, FROZEN, NDOF, STATUS )

*  Description:
*     The number of degrees of freedom is given by the number of good
*     data points less the number of parameters either frozen explicitly
*     or appearing in the model specification constraint list.

*  Arguments:
*     NGOOD = INTEGER (given)
*        Number of valid data points
*     MODEL = /MODEL_SPEC/ (given)
*        Model specification
*     FROZEN = LOGICAL(*) (given)
*        Parameter is frozen?
*     NDOF = INTEGER (returned)
*        Number of degrees of freedom
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Algorithm:
*     {algorithm_description}...

*  References:
*     {routine_references}...

*  Keywords:
*     fitting:constraints,d.o.f

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     16 Dec 1994 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              			! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! Standard SAE constants
      INCLUDE 'DAT_PAR'					! HDS constants
      INCLUDE 'FIT_PAR'		 			! ASTERIX fit constants

*  Structure definitions:
      INCLUDE 'FIT_STRUC'				! ASTERIX fit structures

*  Arguments Given:
      INTEGER			NGOOD			! # of good data points
      RECORD /MODEL_SPEC/ 	MODEL			! Model specification
      LOGICAL			FROZEN(*)		! Frozen parameters?

*  Arguments Returned:
      INTEGER			NDOF			! # degrees of freedom

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			J			! Loop over parameters
*.

*    Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initially the number of good data values less the number of parameters
      NDOF = NGOOD - MODEL.NPAR

*    Loop over parameters
      DO J = 1, MODEL.NPAR

*      Parameter frozen?
        IF ( FROZEN(J) ) THEN

          NDOF = NDOF + 1

*      Ties present?
	ELSE IF ( MODEL.NTIE .GT. 0 ) THEN

*        In a tie group?
          IF ( MODEL.TGROUP(J) .GT. 0 ) THEN

*          Not the base parameter of the group
            IF ( MODEL.TSTART(MODEL.TGROUP(J)) .NE. J ) THEN
              NDOF = NDOF + 1
            END IF

*        End of tie group test
          END IF

*      End of frozen test
        END IF

*    End of parameter loop
      END DO

*    Check ok
      IF ( NDOF .EQ. 0 ) THEN
	CALL MSG_PRNT('Number of degrees of freedom is zero')

      ELSE IF ( NDOF .LT. 0 ) THEN
	STATUS = SAI__ERROR
	CALL ERR_REP( 'BADNDOF', 'More free parameters than'/
     :                /' data values!', STATUS )
      END IF

      END
