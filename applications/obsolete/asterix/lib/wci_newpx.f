      SUBROUTINE WCI_NEWPX( DIMS, BASES, SCALES, UNITS, ROTA,
     :                      ID, STATUS )
*+
*  Name:
*     WCI_NEWPX

*  Purpose:
*     Create a pixellation object describing regular pixellisation

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI_NEWPX( DIMS, BASES, SCALES, UNITS, ROTA, ID, STATUS )

*  Description:
*     Creates a linear pixel to native spherical coordinate mapping. The
*     pixels describe a grid linearly spaced in both axes centred on a
*     special point in the standard coordinate system. The grid may be
*     rotated by an amount ROTA with respect to the standard system.

*  Arguments:
*     DIMS[2] = INTEGER (given)
*        Number of pixels in each axis
*     BASES[2] = REAL (given)
*        Local coordinate value in axis units at centre of first pixel
*     SCALES[2] = REAL (given)
*        Pixel widths in axis units at reference point
*     UNITS[2] = CHARACTER*(*) (given)
*        Unit strings
*     ROTA = DOUBLE (given)
*        Rotation of supplied Y axis from latitude like axis, through +ve
*        longitude axis (ie. position angle), in degrees
*     ID = INTEGER (returned)
*        The ADI identifier of the new Projection object
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

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     WCI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/wci.html

*  Keywords:
*     package:wci, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     4 Jan 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          	! Standard SAE constants
      INCLUDE 'WCI_PAR'          	! ASTERIX WCI constants

*  Global Variables:
      INCLUDE 'WCI_CMN'			! ASTERIX WCI common block
*       WCS_INIT = LOGICAL (given)
*         WCI class definitions loaded?

*  Arguments Given:
      INTEGER			DIMS(2)			! Grid dimensions
      REAL			BASES(2)		! Axis origins
      REAL			SCALES(2)		! Pixel widths
      CHARACTER*(*)		UNITS(2)		! Units in each axis
      DOUBLE PRECISION		ROTA			! Rotation of Y axis

*  Arguments Returned:
      INTEGER			ID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			WCI1_BLK		! Common block init

*  Local variables:
      DOUBLE PRECISION		UCONV(2)		! Unit conversions
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. WCI_INIT ) CALL WCI1_INIT( STATUS )

*  Find axis units to radians conversion
      CALL WCI1_UNIT2R( UNITS(1), UCONV(1), STATUS )
      CALL WCI1_UNIT2R( UNITS(2), UCONV(2), STATUS )

*  Create new instance of Pixellation
      CALL ADI_NEW0( 'Pixellation', ID, STATUS )

*  Write attributes
      CALL ADI_CPUT1I( ID, 'DIMS', 2, DIMS, STATUS )
      CALL ADI_CPUT1R( ID, 'BASE', 2, BASES, STATUS )
      CALL ADI_CPUT1R( ID, 'SCALE', 2, SCALES, STATUS )
      CALL ADI_CPUT1D( ID, 'UCONV', 2, UCONV, STATUS )
      CALL ADI_CPUT0D( ID, 'ROTATION', ROTA, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'WCI_NEWPX', STATUS )

      END
