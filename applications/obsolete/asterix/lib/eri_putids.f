      SUBROUTINE ERI_PUTIDS( ID, INDEX, NRESP, RMFID, ARFID, STATUS )
*+
*  Name:
*     ERI_PUTIDS

*  Purpose:
*     Write energy response to a file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI_PUTIDS( ID, INDEX, NRESP, RMFID, ARFID, STATUS )

*  Description:
*     Writes energy response and area response to a file.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of the dataset object
*     INDEX = INTEGER (given)
*        Response number for this dataset
*     NRESP = INTEGER (given)
*        Total number of responses for dataset
*     RMFID = INTEGER (given)
*        ADI identifier of the RestributionMatrix derived object
*     ARFID = INTEGER (given)
*        ADI identifier of the AreaResponse derived object
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
*     ERI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/eri.html

*  Keywords:
*     package:eri, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     27 Feb 1995 (DJA):
*        Original version.
*     15 Jan 1996 (DJA):
*        Added INDEX and NRESP arguments
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_ERR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'AST_PKG'

*  Arguments Given:
      INTEGER                   ID, INDEX, NRESP, RMFID, ARFID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External references:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI

*  Local Variables:
      INTEGER			IARG(5)			! Method inputs
      INTEGER                   RESID                   ! Method output data

      LOGICAL			DONE			! Written yet?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( ERI__PKG ) ) CALL ERI0_INIT( STATUS )

*  Dataset is always first argument
      CALL ADI_GETFILE( ID, IARG(1), STATUS )

*  Response indices
      CALL ADI_NEWV0I( INDEX, IARG(2), STATUS )
      CALL ADI_NEWV0I( NRESP, IARG(3), STATUS )

*  Haven't succeeded yet
      DONE = .FALSE.

*  Try to invoke method to write both RMF and ARF if both are defined
      IF ( (RMFID .NE. ADI__NULLID) .AND. (ARFID.NE.ADI__NULLID) ) THEN

*    New error context
        CALL ERR_BEGIN( STATUS )

*    Set up method args
        IARG(4) = RMFID
        IARG(5) = ARFID

*    Try the method
        CALL ADI_EXEC( 'Write', 5, IARG, RESID, STATUS )

*    If status is ADI__NOMTH then cancel the status and pretend we'd never
*    tried the method
        IF ( STATUS .EQ. ADI__NOMTH ) THEN
          CALL ERR_ANNUL( STATUS )
        ELSE
          DONE = .TRUE.
        END IF

*    Restore error context
        CALL ERR_END( STATUS )

      END IF

*  If not written in one fell swoop, write individually
      IF ( .NOT. DONE ) THEN

*    The redistribution
        IF ( RMFID .NE. ADI__NULLID ) THEN
          IARG(4) = RMFID
          CALL ADI_EXEC( 'WriteRMF', 4, IARG, RESID, STATUS )
        END IF

*    The ancillary response
        IF ( ARFID .NE. ADI__NULLID ) THEN
          IARG(4) = ARFID
          CALL ADI_EXEC( 'WriteARF', 4, IARG, RESID, STATUS )
        END IF

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ERI_PUTIDS', STATUS )

      END
