      SUBROUTINE GCB2_LOAD( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     GCB2_LOAD

*  Purpose:
*     Load the graphics control block (GCB) from a FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GCB2_LOAD( NARG, ARGS, OARG, STATUS )

*  Description:
*     The partially compressed GCB is read from the GCB extension of the
*     specified FITS file, uncompressed and installed as the current GCB.
*     If an error occurs, it is annulled and the current GCB reset.

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     package:gcb, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     26 Jul 1995 (DJA):
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
      INCLUDE 'GCB_PAR'

*  Global Variables:
      INCLUDE 'GCB_CMN'                                 ! GCB globals
*        G_MEMPTR = INTEGER (given)
*           Active GCB data area

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			GCBDAT			! ASTERIX GCB hdu data
      INTEGER			GCBPTR			! Workspace GCB
      INTEGER			NBYTE			! Length of GCB

      LOGICAL			DIDCRE			! Did we create data?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the GCB hdu data
      CALL ADI2_CFIND( ARGS(1), 'GCB', '@', ' ', .FALSE., .FALSE.,
     :                 'BYTE', 0, 0, DIDCRE, GCBDAT, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Map the HDU data
        CALL ADI2_DCOP_IN( GCBDAT, GCBPTR, NBYTE, STATUS )

*    If no error, attempt to load data
        IF ( STATUS .EQ. SAI__OK ) THEN

*      Unpack it
          CALL GCB_LOAD_SUB( %VAL(GCBPTR), %VAL(G_MEMPTR), STATUS )

*      Free the HDU data - it would be nicer if this was wrapped up
          CALL ADI_CUNMAP( GCBDAT, 'Value', GCBPTR, STATUS )
          CALL ADI_ERASE( GCBDAT, STATUS )

        END IF

      END IF

*  If failed to load, clear the GCB
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        CALL GCB_CLEAR( STATUS )
      END IF

*  No return data
      OARG = ADI__NULLID

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GCB2_LOAD', STATUS )

      END
