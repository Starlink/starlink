      SUBROUTINE WCI2_WRITFIT( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     WCI2_WRITFIT

*  Purpose:
*     Write WCS info to a FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI2_WRITFIT( NARG, ARGS, OARG, STATUS )

*  Description:
*     Writes the WCS information described in the 2nd to 4th arguments to
*     the dataset decsribed by the first.

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
      INCLUDE 'WCI_PAR'					! WCI constants

*  Arguments Given:
      INTEGER			NARG, ARGS(*)

*  Arguments Returned:
      INTEGER			OARG

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the keywords
      CALL WCI2_WRITE_HDU( ARGS(2), 'PRIMARY', ARGS(3), ARGS(4),
     :                     ARGS(5), STATUS )

*  Result is null
      OARG = ADI__NULLID

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'WCI2_WRITFIT', STATUS )
      END IF

      END



      SUBROUTINE WCI2_WRITE_HDU( FID, HDU, PIXID, PRJID, SYSID, STATUS )
*+
*  Name:
*     WCI2_WRITE_HDU

*  Purpose:
*     Write WCS keywords to an HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI2_WRITE_HDU( FID, HDU, PIXID, PRJID, SYSID, STATUS )

*  Description:
*     Writes the WCS information to a particular HDU

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FITSfile object
*     HDU = CHARACTER*(*) (given)
*        Name of HDU to write keywords
*     PIXID = INTEGER (given)
*        Pixellation object
*     PRJID = INTEGER (given)
*        Projection object
*     SYSID = INTEGER (given)
*        Celestial system object
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
      INCLUDE 'WCI_PAR'					! WCI constants

*  Arguments Given:
      INTEGER			FID, PIXID, PRJID, SYSID
      CHARACTER*(*)		HDU

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			SLA_EPJ2D
        DOUBLE PRECISION	SLA_EPJ2D

*  Local Variables:
      CHARACTER*80		LABEL			! X,Y axis labels
      CHARACTER*3		PRJ			! Projection name
      CHARACTER*3		SYS			! Coord system name
      CHARACTER*40		UNITS(2)		! X,Y axis units

      DOUBLE PRECISION		BTAI			! Value of BASE_TAI
      DOUBLE PRECISION		EPOCH			! Epoch
      DOUBLE PRECISION		MJD			! Observation time
      DOUBLE PRECISION		PA			! Position angle
      DOUBLE PRECISION		SPOINT(2)		! RA, DEC

      REAL			BASE(2), SCALE(2)	! Axis values
      REAL			EQNX			! Equinox
      REAL			TOR			! Radian conversion

      INTEGER			DIMS(2)			! Axis dimensions
      INTEGER			HDUID			! HDU id
      INTEGER			IMJD			! Value of BASE_MJD
      INTEGER			IPSF			! Psf system handle
      INTEGER			PTR(2)			! Axis data pointers
      INTEGER			X_AX,Y_AX,E_AX,T_AX	! Axis numbers

      LOGICAL			EQOK, EPOK		! Equinox & epoch ok?
      LOGICAL			HASPIX			! Spatial axes exist?
      LOGICAL			IPSFOK			! Psf id exists?
      LOGICAL			RAOK, DECOK, PAOK       ! Found ok flags
      LOGICAL			REG(2)			! Axes regular
      LOGICAL			PRJOK, SYSOK		! Projection/system ok?
      LOGICAL			TAIOK			! BASE_TAI found?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the named HDU
      CALL ADI2_FNDHDU( FID, HDU, .TRUE., HDUID, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Get coordinate system details
        IF ( SYSID .NE. ADI__NULLID ) THEN

*      Get system name
          CALL ADI_CGET0C( SYSID, 'NAME', SYS, STATUS )
          CALL ADI2_HPKYC( HDUID, 'RADECSYS', SYS,
     :                     'World coordinate system', STATUS )

*      Get equinox
          CALL ADI_CGET0R( SYSID, 'EQUINOX', EQNX, STATUS )
          CALL ADI2_HPKYR( HDUID, 'EQUINOX', EQNX,
     :                    'Epoch of mean equator & equinox', STATUS )

*      Epoch. Don't write if default was supplied
          CALL ADI_CGET0D( SYSID, 'EPOCH', EPOCH, STATUS )
          IF ( EPOCH .NE. WCI__FLAG ) THEN
            CALL ADI2_HPKYD( HDUID, 'MJD-OBS', SLA_EPJ2D( EPOCH ),
     :                      'MJD of observation', STATUS )
          END IF

        END IF

*    Release the HDU
        CALL ADI_ERASE( HDUID, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'WCI2_WRITE_HDU', STATUS )
      END IF

      END
