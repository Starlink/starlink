      SUBROUTINE WCI_CNS2A( SPOS, PIXID, PRJID, APOS, STATUS )
*+
*  Name:
*     WCI_CNA2S

*  Purpose:
*     Convert position in standard system to axis units

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI_CNS2A( SPOS, PIXID, PRJID, APOS, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     SPOS[2] = DOUBLE (given)
*        Celestial position in radians
*     PIXID = INTEGER (given)
*        Pixellation object
*     PRJID = INTEGER (given)
*        Projection object
*     APOS[2] = REAL (returned)
*        Position in axis units relative to reference point
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
*     WCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/wci.html

*  Keywords:
*     package:wci, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

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
      INCLUDE 'MATH_PAR'
      INCLUDE 'AST_PKG'

*  Arguments Given:
      DOUBLE PRECISION		SPOS(2)
      INTEGER			PIXID, PRJID

*  Arguments Returned:
      REAL			APOS(2)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			AST_QPKGI
        LOGICAL			AST_QPKGI

*  Local Variables:
      DOUBLE PRECISION		LAPOS(2)		! Position in axis units
      DOUBLE PRECISION		RPH(2)			! Relative physical posn
      DOUBLE PRECISION          RPHP(2)			! Projected coords
      DOUBLE PRECISION          RPHV(3)			! Cartesian RPH
      DOUBLE PRECISION          STDV(3)			! Cartesian SPOS
      DOUBLE PRECISION		UCONV(2)		! Unit conversions

      INTEGER			DDIM			! # ADI values fetched
      INTEGER			RPTR			! Rotation matrix ptr
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( WCI__PKG ) ) CALL WCI1_INIT( STATUS )

*  Map the rotation matrix
      CALL ADI_CMAPD( PRJID, 'RMATRIX', 'READ', RPTR, STATUS )

*  Generate 3-vector for SPOS
      CALL SLA_DCS2C( SPOS(1), SPOS(2), STDV )

*  Rotate it back to native sphericals
      CALL SLA_DMXV( %VAL(RPTR), STDV, RPHV )

*  Release rotation matrix
      CALL ADI_CUNMAP( PRJID, 'RMATRIX', RPTR, STATUS )

*  Extract native sphericals
      CALL SLA_DCC2S( RPHV, RPH(1), RPH(2) )

*  Feed through map projection
      CALL WCI_PROJ( RPH, PRJID, RPHP, STATUS )

*  Rotate from native sphericals to axis units
      CALL WCI1_ROTA( RPHP, PIXID, .TRUE., LAPOS, STATUS )

*  Restore axis conversion factors
      CALL ADI_CGET1D( PIXID, 'UCONV', 2, UCONV, DDIM, STATUS )
      APOS(1) = LAPOS(1) / UCONV(1)
      APOS(2) = LAPOS(2) / UCONV(2)

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'WCI_CNS2A', STATUS )

      END
