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
*     WCI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/wci.html

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
      INTEGER			NARG			! # arguments
      INTEGER			ARGS(*)			! Method arguments

*  Arguments Returned:
      INTEGER			OARG			! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			SLA_EPJ
        DOUBLE PRECISION	SLA_EPJ

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	HLOC			! Object header
      CHARACTER*80		LABEL			! X,Y axis labels
      CHARACTER*(DAT__SZLOC)	LOC			! Object locator
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
      INTEGER			IMJD			! Value of BASE_MJD
      INTEGER			IPSF			! Psf system handle
      INTEGER			PIXID			! Pixellation object
      INTEGER			PRJID			! Projection object
      INTEGER			PTR(2)			! Axis data pointers
      INTEGER			SYSID			! CoordSystem object
      INTEGER			X_AX,Y_AX,E_AX,T_AX	! Axis numbers

      LOGICAL			HASPIX			! Spatial axes exist?
      LOGICAL			REG(2)			! Axes regular
      LOGICAL			PRJOK, SYSOK		! Projection/system ok?
      LOGICAL			TAIOK			! BASE_TAI found?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the locator
      CALL ADI1_GETLOC( ARGS(1), LOC, STATUS )

*  Get 3 structures from argument array
      PIXID = ARGS(2)
      PRJID = ARGS(3)
      SYSID = ARGS(4)

*  Look for ASTERIX header data
      CALL ADI1_LOCHEAD( ARGS(1), .TRUE., HLOC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Write data to header
        IF ( PRJID .NE. ADI__NULLID ) THEN

*      The pointing direction
          CALL ADI_CGET1D( PRJID, 'SPOINT', 2, SPOINT, NACT, STATUS )
          CALL HDX_PUTD( HLOC, 'AXIS_RA', 1, SPOINT(1), STATUS )
          CALL HDX_PUTD( HLOC, 'AXIS_DEC', 1, SPOINT(2), STATUS )

        END IF

        IF ( SYSID .NE. ADI__NULLID ) THEN

*      Equinox
          CALL ADI_CGET0R( SYSID, 'EQUINOX', EQNX, STATUS )
          CALL HDX_PUTR( HLOC, 'EQUINOX', 1, EQNX, STATUS )

        END IF

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'WCI1_WRITE', STATUS )
      END IF

      END
