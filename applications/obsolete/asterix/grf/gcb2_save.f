      SUBROUTINE GCB2_SAVE( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     GCB2_SAVE

*  Purpose:
*     Saves Grafix Control Block to FITS file object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GCB2_SAVE( NARG, ARGS, OARG, STATUS )

*  Description:
*     {routine_description}

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
*     19 Jul 1995 (DJA):
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
*        G_MEMPTR = INTEGER (given)
*           Active GCB data area
*        G_VERSION = REAL (given)
*           GCB version number

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			GCBPTR
      INTEGER			NBYTE
      INTEGER			NSCAL
      INTEGER			NSTRUC
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find size required
      CALL GCB_CSIZE( NBYTE, NSCAL, NSTRUC, STATUS )

*  Create workspace
      CALL DYN_MAPB( 1, NBYTE, GCBPTR, STATUS )

*  Copy semi-compressed GCB to workspace
      CALL GCB_SAVE_SUB( NSCAL, NSTRUC, %VAL(G_MEMPTR), %VAL(GCBPTR),
     :                                                       STATUS )

*  Define an image extension
      CALL ADI2_DEFIMG( ARGS(1), 'GCB', 1, NBYTE, 'BYTE', STATUS )

*  Write keywords to define contents of HDU
      CALL ADI2_PKEY0C( ARGS(1), 'GCB', 'CONTENT', 'GRAFIX CONTROL',
     :                  'Version of GCB description', STATUS )
      CALL ADI2_PKEY0R( ARGS(1), 'GCB', 'GCBVERSN', G_VERSION,
     :                  'Version of GCB description', STATUS )

*  Write the data to the HDU
      CALL ADI2_PUTIMGB( ARGS(1), 'GCB', 1, NBYTE, %VAL(GCBPTR),
     :                   STATUS )

*  Free the workspace
      CALL DYN_UNMAP( GCBPTR, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GCB2_SAVE', STATUS )

      END
