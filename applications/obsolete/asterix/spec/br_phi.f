      DOUBLE PRECISION FUNCTION BR_PHI( A, STATUS )
*+
*  Name:
*     BR_PHI

*  Purpose:
*     Gould's bremmstrahlung Born approximation correction

*  Language:
*     Starlink Fortran

*  Invocation:
*     RESULT = BR_PHI( A, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     A = DOUBLE PRECISION (given)
*        {argument_description}
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Returned Value:
*     BR_PHI = DOUBLE PRECISION
*        Gould's bremmstrahlung Born approximation correction

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {facility_or_package}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  Keywords:
*     package:br, usage:public

*  Copyright:
*     {routine_copyright}

*  Authors:
*     AMTP : Anndy Pollock (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     20 Aug 1985 (AMPT):
*        Original version
*     14 Jul 1988 (RDJ):
*        Amended to cope with high values of E/kT
*     13 Jan 1993 (DJA):
*        Converted to D.P.
*      7 Feb 1996 (DJA):
*        Use PDA routine rather than NAG
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'

*  Arguments Given:
      DOUBLE PRECISION			A

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL				BR_DEXP
        DOUBLE PRECISION 		BR_DEXP
      EXTERNAL				BR_K0
        DOUBLE PRECISION 		BR_K0
      EXTERNAL				BR_K1
        DOUBLE PRECISION 		BR_K1
      EXTERNAL				PDA_DERFC
        DOUBLE PRECISION 		PDA_DERFC

*  Local Variables:
      DOUBLE PRECISION 		EMA,ERFC       		!
      INTEGER			PFAIL               	! PDA failure code
*.

*  Check inherited global status
      IF ( STATUS .EQ. SAI__OK ) THEN

        PFAIL = 0
        ERFC = PDA_DERFC( SQRT(2.0D0*A), PFAIL )
        IF ( PFAIL .EQ. 0 ) THEN
          IF ( A .LT. 40.0 ) THEN
            EMA = BR_DEXP(-A)
            RVAL = MATH__DPI*((1.+SQRT(MATH__DPI*A))*ERFC-
     :             EMA*EMA/SQRT(2.))/
     :             (2.*A*EMA*(BR_K1(A,STATUS)-BR_K0(A,STATUS)))

*      Approximate expression used at high A values
          ELSE
            RVAL = 1.0D0-(SQRT(MATH__DPI/A)/4.)+(0.125/A)
          END IF

        ELSE
          RVAL = 0.0D0
          STATUS = SAI__ERROR
          CALL ERR_REP( 'PDA_ERROR',
     :              'PFAIL non-zero on exit from PDA_DERFC', STATUS )
        END IF

      ELSE
        RVAL = 0.0D0

      END IF

*  Set return value
      BR_PHI = RVAL

      END
