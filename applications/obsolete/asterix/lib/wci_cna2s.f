      SUBROUTINE WCI_CNA2S( APOS, PIXID, PRJID, SPOS, STATUS )
*+
*  Name:
*     WCI_CNA2S

*  Purpose:
*     Convert position in axis units to standard system

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI_CNA2S( APOS, PIXID, PRJID, SPOS, STATUS )

*  Description:
*     Convert the position APOS in native axis units to standard coordinates
*     in radians. The position is first de-rotated if the position angle
*     defined in the pixellation object is non-zero, then the map projection
*     specified by PRJID is supplied.

*  Arguments:
*     APOS[2] = REAL (given)
*        Position in fractional pixels. 1.0 is the centre of the first pixel
*     PIXID = INTEGER (given)
*        Pixellation object
*     PRJID = INTEGER (given)
*        Projection object
*     SPOS[2] = DOUBLE (returned)
*        Celestial position in radians
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
*     - WCI1_INIT must have been run (implicitly by calls to public WCI
*       routines which create ADI identifiers)

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     ADI:
*        ADI_GET/MAP	Various ADI data access routines
*     SLA:
*        SLA_DCS2C      Spherical to Cartesian conversion
*        SLA_DCC2S      Cartesian to spherical conversion

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     WCI Subroutine Guide : http://www.star.sr.star.ac.uk:8080/asterix-docs/Programmer/Guides/wci.html

*  Keywords:
*     package:wci, usage:public, coordinate conversion

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     5 Jan 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'WCI_CMN'                 ! ASTERIX WCI common block
*       WCS_INIT = LOGICAL (given)
*         WCI class definitions loaded?

*  Arguments Given:
      REAL			APOS(2)			! Axis position
      INTEGER			PIXID			! Pixellation
      INTEGER			PRJID			! Projection details

*  Arguments Returned:
      DOUBLE PRECISION		SPOS(2)			! Celestial position

*  Status:
      INTEGER 			STATUS             	! Global status

*  External references:
      EXTERNAL			SLA_DRANGE
        DOUBLE PRECISION	SLA_DRANGE
      EXTERNAL			SLA_DRANRM
        DOUBLE PRECISION	SLA_DRANRM

*  Local Variables:
      DOUBLE PRECISION		LAPOS(2)		! Position in axis units
      DOUBLE PRECISION		RPH(2)			! Relative physical posn
      DOUBLE PRECISION          RPHP(2)			! Projected coords
      DOUBLE PRECISION          RPHV(3)			! Cartesian RPH
      DOUBLE PRECISION          STDV(3)			! Cartesian SPOS
      DOUBLE PRECISION		UCONV(2)		! Unit conversions

      INTEGER			DDIM			! Dummy dimensions
      INTEGER			RPTR			! Ptr to rotation matrix
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. WCI_INIT ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'WCI has not been initialised', STATUS )
      END IF

*  Extract radian conversions for axes
      CALL ADI_CGET1D( PIXID, 'UCONV', 2, UCONV, DDIM, STATUS )

*  Convert axis position to radians
      LAPOS(1) = DBLE(APOS(1)) * UCONV(1)
      LAPOS(2) = DBLE(APOS(2)) * UCONV(2)

*  De-rotate. This is now relative physical units wrt to the projection
*  special point
      CALL WCI1_ROTA( LAPOS, PIXID, .FALSE., RPHP, STATUS )

*  De-project to get native sphericals
      CALL WCI_UNPROJ( RPHP, PRJID, RPH, STATUS )

*  Get rotation matrix from projection
      CALL ADI_CMAPD( PRJID, 'RMATRIX', 'READ', RPTR, STATUS )

*  Generate 3-vector for RPHP
      CALL SLA_DCS2C( RPH(1), RPH(2), RPHV )

*  Rotate it
      CALL SLA_DIMXV( %VAL(RPTR), RPHV, STDV )

*  Unmap matrix
      CALL ADI_CUNMAP( PRJID, 'RMATRIX', RPTR, STATUS )

*  Extract output position
      CALL SLA_DCC2S( STDV, SPOS(1), SPOS(2) )

*  Force into range
      SPOS(1) = SLA_DRANRM( SPOS(1) )
      SPOS(2) = SLA_DRANGE( SPOS(2) )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'WCI_CNA2S', STATUS )

      END
