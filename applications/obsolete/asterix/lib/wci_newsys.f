      SUBROUTINE WCI_NEWSYS( NAME, EQNX, EPOCH, ID, STATUS )
*+
*  Name:
*     WCI_NEWSYS

*  Purpose:
*     Create a new coordinate system object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI_NEWSYS( NAME, EQNX, EPOCH, ID, STATUS )

*  Description:
*     Creates a coordinate system object which fully defines an absolute
*     celestial coordinate system.

*  Arguments:
*     NAME = CHARACTER*(*) (given)
*        Name of the coordinate system. Can take values FK4, FK5, ECLIPTIC,
*        GALACTIC or SUPERGALACTIC, or any abbreviation thereof.
*     EQNX = REAL (given)
*        The epoch of the mean equator and equinox in years.
*     EPOCH = DOUBLE (given)
*        Epoch of the observation in years. If the value WCI__FLAG is
*        supplied then the year of the EQNX is used.
*     ID = INTEGER (returned)
*        The ADI identifier of the new CoordSystem object
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     CALL WCI_NEWSYS( 'FK4', 1950.0, WCI__FLAG, ID, STATUS )
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
      CHARACTER*(*)		NAME
      REAL			EQNX
      DOUBLE PRECISION  	EPOCH

*  Arguments Returned:
      INTEGER			ID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			WCI1_BLK		! Common block init

*  Local variables:
      CHARACTER*1		EFORM			! Epoch code
      CHARACTER*3		LNAME			! Coord system name

      DOUBLE PRECISION		LEPOCH			! Local epoch
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. WCI_INIT ) CALL WCI1_INIT( STATUS )

*  Check system name
      CALL WCI1_CHKSNM( NAME, STATUS )

*  Create new instance of CoordSystem
      CALL ADI_NEW0( 'CoordSystem', ID, STATUS )

*  Store attributes
      LNAME = NAME
      CALL CHR_UCASE( LNAME )
      CALL ADI_CPUT0C( ID, 'NAME', LNAME, STATUS )
      CALL ADI_CPUT0R( ID, 'EQUINOX', EQNX, STATUS )

*  Use EQNX for the epoch if EPOCH has the flag value
      IF ( EPOCH .EQ. WCI__FLAG ) THEN
        LEPOCH = EQNX
      ELSE
        LEPOCH = EPOCH
      END IF

*  Choose the epoch type, Besselian or Julian
      IF ( LNAME .EQ. 'FK4' ) THEN
        EFORM = 'B'
      ELSE IF ( LNAME .EQ. 'FK5' ) THEN
        EFORM = 'J'
      ELSE
        IF ( LEPOCH .LT. 1984D0 ) THEN
          EFORM = 'J'
        ELSE
          EFORM = 'J'
        END IF
      END IF
      CALL ADI_CPUT0C( ID, 'EFORM', EFORM, STATUS )

*  Convert epoch to years if flag value not used
      CALL ADI_CPUT0D( ID, 'EPOCH', LEPOCH, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'WCI_NEWSYS', STATUS )

      END
