      SUBROUTINE SFIT_PRECHK( NDS, Z, STATUS )
*+
*  Name:
*     SFIT_PRECHK

*  Purpose:
*     Apply red-shift to model space energy bounds and check structures

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL SFIT_PRECHK( NDS, Z, PREDDAT, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NDS = INTEGER (given)
*        Number of datasets
*     Z = REAL (given)
*        The red-shift
*     PREDDAT[] = RECORD /PREDICTION/ (given)
*        The data structure describing the predicted data
*     STATUS = INTEGER (given)
*        The global status.

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
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     SFIT Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/sfit.html

*  Keywords:
*     package:sfit, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      1 Dec 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIT_PAR'

*  Structure Definitions:
      INCLUDE 'FIT_STRUC'

*  Arguments Given:
      INTEGER			NDS
      REAL			Z
c     RECORD /PREDICTION/    	PREDDAT(NDS)

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      INTEGER			N			! Loop over datasets
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over datasets
      DO N = 1, NDS

*    Apply redshift to model space energy bounds
        CALL SFIT_APPRED( Z, PREDICTION_NMBOUND(N),
     :                    %VAL(PREDICTION_MLBNDPTR(N)),
     :                    %VAL(PREDICTION_MUBNDPTR(N)), STATUS )

*    Report on success in finding instrument response if appropriate
	IF ( PREDICTION_CONVOLVE(N) ) THEN
	  IF ( NDS .EQ. 1 ) THEN
	    CALL MSG_PRNT('Instrument response found')
          ELSE
	    CALL MSG_SETI('NDS',N)
	    CALL MSG_PRNT('Instrument response found for'//
     :          ' dataset ^NDS')
	  END IF
        ELSE
	  IF ( NDS .EQ. 1 ) THEN
	    CALL MSG_PRNT('!! Warning - no instrument response found'//
     :          ', results may not be meaningful !!')
          ELSE
	    CALL MSG_SETI( 'NDS', N )
	    CALL MSG_PRNT('!! Warning - no instrument response for '//
     :          'dataset ^NDS !!')
	  END IF
        END IF

      END DO

*  Flush any warnings
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_FLUSH( STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'SFIT_PRECHK', STATUS )
      END IF

      END
