      SUBROUTINE WCI1_WRITE( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     WCI1_WRITE

*  Purpose:
*     Write the WCS info to an HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI1_WRITE( NARG, ARGS, OARG, STATUS )

*  Description:
*     Constructs the data objects required by WCI from the supplied HDS
*     dataset. This may be any Starlink NDF, but if an ASTERIX dataset
*     is supplied then additional astrometic information can be extracted.

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
*     Creates a PSF_SLOT property on the property list of the first
*     argument if one is not already present.

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     SLA:
*        SLA_EPJ	- MJD to Julian epoch conversion

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     WCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/wci.html

*  Keywords:
*     package:wci, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'ADI_PAR'					! ADI constants
      INCLUDE 'DAT_PAR'					! HDS constants
      INCLUDE 'WCI_PAR'					! WCI constants

*  Arguments Given:
      INTEGER			NARG, ARGS(*)

*  Arguments Returned:
      INTEGER			OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	HLOC			! Object header

      DOUBLE PRECISION		SPOINT(2)		! RA, DEC

      INTEGER			NACT			! Actual # values read
      INTEGER			PIXID			! Pixellation object
      INTEGER			PRJID			! Projection object
      INTEGER			SYSID			! CoordSystem object
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get 3 structures from argument array
      PIXID = ARGS(3)
      PRJID = ARGS(4)
      SYSID = ARGS(5)

*  Look for ASTERIX header data
      CALL ADI1_LOCHEAD( ARGS(2), .TRUE., HLOC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Stuff in the pixellation description structure
        IF ( PIXID .NE. ADI__NULLID ) THEN

*      Position angle
          CALL ADI1_CCA2HD( PIXID, 'ROTATION', HLOC, 'POSITION_ANGLE',
     :                      STATUS )

        END IF

*    Stuff in projection object
        IF ( PRJID .NE. ADI__NULLID ) THEN

*      The pointing direction
          CALL ADI_CGET1D( PRJID, 'SPOINT', 2, SPOINT, NACT, STATUS )
          CALL HDX_PUTD( HLOC, 'AXIS_RA', 1, SPOINT(1), STATUS )
          CALL HDX_PUTD( HLOC, 'AXIS_DEC', 1, SPOINT(2), STATUS )

*      The nominal pointing direction
          CALL ADI_CGET1D( PRJID, 'NPOINT', 2, SPOINT, NACT, STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL HDX_PUTD( HLOC, 'FIELD_RA', 1, SPOINT(1), STATUS )
            CALL HDX_PUTD( HLOC, 'FIELD_DEC', 1, SPOINT(2), STATUS )
          ELSE
            CALL ERR_ANNUL( STATUS )
          END IF

        END IF

*    Stuff in the coordinate system description structure
        IF ( SYSID .NE. ADI__NULLID ) THEN

*      Equinox
          CALL ADI1_CCA2HD( SYSID, 'EQUINOX', HLOC, 'EQUINOX', STATUS )

        END IF

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'WCI1_WRITE', STATUS )
      END IF

      END
