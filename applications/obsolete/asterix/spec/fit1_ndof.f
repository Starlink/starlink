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

*  External References:
      [external_declaration]
      {data_type} {external_name} ! [external_description]

*    Local Variables:
      INTEGER			J			! Loop over parameters
      INTEGER			ITIE			! Loop over ties
*.

*    Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initially the number of good data values
      NDOF = NGOOD

*    Less the frozen parameters
      DO J = 1, NPAR
	IF ( .NOT. FROZEN(J) ) NDOF = NDOF - 1
      END DO

*    Less the constrained parameters
      IF ( MODEL.NTIE .GT. 0 ) THEN

*      Loop over parameters
        DO J = 1, NPAR

*        Parameter not already frozen
	  IF ( .NOT. FROZEN(J) ) THEN

*          In a tie group?
            IF ( MODEL.TGROUP(J) .GT. 0 ) THEN

*            Not the base parameter of the group
              IF ( MODEL.TSTART(MODEL.TGROUP(J)) .NE. J ) NDOF = NDOF - 1

            END IF
          END IF
        END DO
      END IF

*    Check ok
      IF ( NDOF .EQ. 0 ) THEN
	CALL MSG_PRNT('Number of degrees of freedom is zero')

      ELSE IF ( NDOF .LT. 0 ) THEN
	STATUS = SAI__ERROR
	CALL ERR_REP( 'BADNDOF', 'More free parameters than'/
     :                /' data values!', STATUS )
      END IF

      END
