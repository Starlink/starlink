      SUBROUTINE FIT_PRGRS( NIT, PARAM, FROZEN, PEGGED, MODEL,
     :                      FINISHED, STATUS )
*+
*  Name:
*     FIT_PRGRS

*  Purpose:
*     Report progress of fit and update model file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FIT_PRGRS( NIT, PARAM, FROZEN, PEGGED, MODEL, FINISHED, STATUS )

*  Description:
*     Issue fit progress report to the console, and then update the model
*     file.

*  Arguments:
*     NIT = INTEGER (given)
*        Iteration number
*     PARAM[] = REAL (given)
*        Parameter values
*     FROZEN[] = LOGICAL (given)
*        Parameters frozen?
*     PEGGED[] = LOGICAL (given)
*        Parameters frozen?
*     MODEL = /MODEL/ (given)
*        Model description
*     FINISHED = LOGICAL (given)
*        Finished fitting?
*     STATUS = INTEGER (given and returned)
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
*     FIT Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fit.html

*  Keywords:
*     package:fit, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     17 Apr 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'FIT_PAR'
      INCLUDE 'FIT_STRUC'

*  Arguments Given:
      INTEGER			NIT
      RECORD /MODEL/		MODEL
      REAL			PARAM(*)
      LOGICAL			FINISHED, FROZEN(*), PEGGED(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*2		SPAR			! String version of int

      INTEGER			J			! Loop over parameters
      INTEGER			NC			! # digits used in SPAR
      INTEGER			NFREE			! # free parameters
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report progress
      CALL MSG_BLNK()
      CALL MSG_SETI( 'NIT', NIT )
      CALL MSG_PRNT( '* Iteration ^NIT *' )
      NFREE = 0
      DO J = 1, MODEL.NPAR

*    Describe parameter state
	IF ( FROZEN(J) ) THEN
          CALL MSG_SETC( 'STATE', 'frozen' )

	ELSE IF ( PEGGED(J) ) THEN
          CALL MSG_SETC( 'STATE', 'pegged' )

	ELSE
          IF ( (MODEL.NTIE.GT.0) .AND. (MODEL.TGROUP(J).GT.0) ) THEN
            IF ( J .NE. MODEL.TSTART(MODEL.TGROUP(J)) ) THEN
              CALL CHR_ITOC( MODEL.TSTART(MODEL.TGROUP(J)), SPAR, NC )
              CALL MSG_SETC( 'STATE', 'free (tied to '/
     :                                     /SPAR(:NC)//')' )
            ELSE
              CALL MSG_SETC( 'STATE', 'free' )
              NFREE = NFREE + 1
            END IF
          ELSE
            CALL MSG_SETC( 'STATE', 'free' )
            NFREE = NFREE + 1
          END IF
	END IF

*    Report parameter
        CALL MSG_FMTR( 'PVAL', '1PG14.6', PARAM(J) )
        CALL MSG_PRNT( MODEL.PARNAME(J)//' ^PVAL   ^STATE' )

*  Next parameter
      END DO

*  Finished yet?
      IF ( FINISHED ) THEN
	CALL MSG_BLNK()
	IF ( NFREE .EQ. 0 ) THEN
	  CALL MSG_PRNT( '+++ No parameters free +++' )
	ELSE
	  CALL MSG_PRNT( '+++ Minimum found +++' )
	END IF
        CALL MSG_BLNK()
      END IF

*  Update model file
      CALL MSG_BLNK()
      CALL MSG_PRNT( '** Updating model spec - do not exit '/
     :                                  /'until completed **' )
      CALL FIT_MODUP( MODEL.M_ID, MODEL.NCOMP, MODEL.NPAR,
     :                PARAM, 0.0, 0.0, -99.0, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
	CALL ERR_FLUSH(STATUS)
      ELSE
	CALL MSG_PRNT( '** Model updated **' )
	CALL MSG_BLNK()
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FIT_PRGRS', STATUS )

      END
