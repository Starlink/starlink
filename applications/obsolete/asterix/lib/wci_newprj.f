      SUBROUTINE WCI_NEWPRJ( NAME, NPAR, PARAM, SPOINT,
     :                       LPOLE, ID, STATUS )
*+
*  Name:
*     WCI_NEWPRJ

*  Purpose:
*     Create a new map projection object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI_NEWPRJ( NAME, NPAR, PARAM, SPOINT, LPOLE, ID, STATUS )

*  Description:
*     Creates a map projection object.

*  Arguments:
*     NAME = CHARACTER*(*) (given)
*        Name of the map projection.
*     NPAR = INTEGER (given)
*        The number of parameters required to specify the projection. This
*        may be zero.
*     PARAM[*] = REAL (given)
*        The projection parameters. Should be declared as array at least as
*        big as NPAR.
*     SPOINT[2] = DOUBLE (given)
*        Position of special point in projection, units are degrees
*     LPOLE = DOUBLE (given)
*        Longitude of the pole of the standard system in the native system
*     ID = INTEGER (returned)
*        The ADI identifier of the new Projection object
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     CALL WCI_NEWPRJ( 'TAN', 0, 0.0, FCENTRE, 180.0, ID, STATUS )
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
*     {routine_copyright}

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
      CHARACTER*(*)		NAME			! Name of projection
      INTEGER			NPAR			! # control params
      REAL			PARAM(*)		! Control parameters
      DOUBLE PRECISION		SPOINT(2)		! Special point
      DOUBLE PRECISION		LPOLE			! Longitude of pole

*  Arguments Returned:
      INTEGER			ID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET
      EXTERNAL			WCI1_BLK		! Common block init

*  Local variables:
      CHARACTER*3		LNAME			! Projection name

      DOUBLE PRECISION		AP, DP			! Position of pole
      DOUBLE PRECISION		CAP, SAP		! Cos(AP), Sin(AP)
      DOUBLE PRECISION		RMAT(3,3)		! Rotation matrix

      INTEGER			RPTR			! Map convertor

*  Local data:
      INTEGER			RDIMS(2)		! Dimensions of RMAT
        DATA			RDIMS/3,3/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. WCI_INIT ) CALL WCI1_INIT( STATUS )

*  Check projection name
      CALL WCI1_CHKPNM( NAME, STATUS )

*  Create new instance of Projection
      CALL ADI_NEW0( 'Projection', ID, STATUS )

*  Store attributes
      LNAME = NAME
      CALL CHR_UCASE( LNAME )
      CALL ADI_CPUT0C( ID, 'NAME', LNAME, STATUS )
      IF ( NPAR .GT. 0 ) THEN
        CALL ADI_CPUT1R( ID, 'PARAM', NPAR, PARAM, STATUS )
      END IF
      CALL ADI_CPUT1D( ID, 'SPOINT', 2, SPOINT, STATUS )
      CALL ADI_CPUT0D( ID, 'LPOLE', LPOLE, STATUS )

*  Locate convertor function and store as property
      CALL WCI1_LOCPRJ( LNAME(1:3), RPTR, STATUS )
      CALL ADI_CPUT0I( ID, '.WCIRTN', RPTR, STATUS )

*  Find the standard system coords of the north pole of the unprojected coords
*  For projections whose special point is the north pole of the native system
*  this is is just the coordinates of the special point. For others it must be
*  calculated
      IF ( CHR_INSET( 'AZP,TAN,SIN,STG,ARC,ZPN,ZEA,AIR,COP,COD,'/
     :                             /'COE,COO,BON', LNAME(1:3) ) ) THEN
        AP = SPOINT(1)
        DP = SPOINT(2)

      ELSE
        DP = ACOSD( SIND(SPOINT(2))/COSD(LPOLE))
        SAP = SIND(LPOLE) / COSD(SPOINT(2))
        CAP = - TAND(DP) * TAND(SPOINT(2))
        AP = SPOINT(1) - ATAN2D(SAP,CAP)
        IF (DP.GT.90D0) DP = -90D0 + (DP-90D0)
        IF (DP.LT.-90D0) DP = 90D0 - (-DP-90D0)

      END IF

*  Generate rotation matrix
      CALL WCI1_GENROT( AP, DP, LPOLE, RMAT, STATUS )

*  Write matrix to object
      CALL ADI_CPUTD( ID, 'RMATRIX', 2, RDIMS, RMAT, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'WCI_NEWPRJ', STATUS )

      END
