      SUBROUTINE GCB_CSAVE( CACHE, FID, STATUS )
*+
*  Name:
*     GCB_CSAVE

*  Purpose:
*     Saves cached Grafix Control Block to file object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GCB_CSAVE( CACHE, FID, STATUS )

*  Description:
*     Saves Grafix Control Block to file object by invoking the SaveGCB
*     method in the FID argument.

*  Arguments:
*     CACHE = INTEGER (given)
*        Address in memory of cached GCB
*     FID = INTEGER (given)
*        ADI identifier of file object
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
*     GCB Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/gcb.html

*  Keywords:
*     package:gcb, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     12 Oct 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GCB_PAR'

*  Global Variables:
      INCLUDE 'GCB_CMN'                                 ! GCB globals
*        G_MTHINIT = LOGICAL (given and returned)
*           GCB definitions load attempted?

*  Arguments Given:
      INTEGER			CACHE, FID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL                  GCB0_BLK                ! Ensures inclusion

*  Local Variables:
      INTEGER			COBJ			! Cache argument
      INTEGER			FILID			! Base file object
      INTEGER                   RESID                   ! Ignored return data
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. G_MTHINIT ) CALL GCB0_INIT( STATUS )

*  Get base file object
      CALL ADI_GETFILE( FID, FILID, STATUS )

*  Create cache address argument
      CALL ADI_NEWV0I( CACHE, COBJ, STATUS )

*  Simply invoke the method
      CALL ADI_EXEC2( 'SaveCachedGCB', COBJ, FILID, RESID, STATUS )

*  Destroy cache argument
      CALL ADI_ERASE( COBJ, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GCB_CSAVE', STATUS )

      END
