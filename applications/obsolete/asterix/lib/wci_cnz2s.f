      SUBROUTINE WCI_CNZ2S( INP, SYS, OUT, STATUS )
*+
*  Name:
*     WCI_CNZ2S

*  Purpose:
*     Convert position in WCI standard system to any chosen system

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI_CNZ2S( INP, SYS, OUT, STATUS )

*  Description:
*     Convert the J2000 FK5 position INP to the coordinate system
*     described by SYS.

*  Arguments:
*     INP[2] = DOUBLE (given)
*        Input position
*     SYS = INTEGER (given)
*        ADI identifier of required output coordinate system
*     OUT[2] = DOUBLE (returned)
*        Output position
*     STATUS = INTEGER (given)
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
*     WCI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/wci.html

*  Keywords:
*     package:wci, usage:public

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
      INTEGER			SYS			! Input coord system
      DOUBLE PRECISION		INP(2)			! Input position

*  Arguments Returned:
      DOUBLE PRECISION		OUT(2)			! Output position

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			SLA_EPCO		! Epoch convertor
        DOUBLE PRECISION	SLA_EPCO
      EXTERNAL			SLA_EPJ2D		! Epoch -> MJD
        DOUBLE PRECISION	SLA_EPJ2D
      EXTERNAL			WCI1_BLK		! Common block init

*  Local Variables:
      CHARACTER*1		EFORM			! Epoch code, B|J
      CHARACTER*3		N3			! System name code

      DOUBLE PRECISION		A, B, AW, BW, DA, DB	! Work space variables
      DOUBLE PRECISION		EPOCH			! Output system epoch
      DOUBLE PRECISION		EQNX			! Output system equinox
      DOUBLE PRECISION		FEQNX			! Final equinox
      DOUBLE PRECISION		FEPOCH			! Final epoch
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. WCI_INIT ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'WCI has not been initialised', STATUS )
      END IF

*  Get the name of the output system
      CALL ADI_CGET0C( SYS, 'NAME', N3, STATUS )

*  Get the required equinox and epoch
      CALL ADI_CGET0D( SYS, 'EQUINOX', EQNX, STATUS )
      CALL ADI_CGET0C( SYS, 'EFORM', EFORM, STATUS )
      CALL ADI_CGET0D( SYS, 'EPOCH', EPOCH, STATUS )

*  Switch on output system
*    This is our standard system
      IF ( N3 .EQ. 'FK5' ) THEN

*    Precess to required epoch
        OUT(1) = INP(1)
        OUT(2) = INP(2)
        CALL SLA_PRECES( 'FK5', 2000D0, SLA_EPCO('J',EFORM,EPOCH),
     :                   OUT(1), OUT(2) )

*    The old system
      ELSE IF ( N3 .EQ. 'FK4' ) THEN

*      Get final epoch
        FEQNX = SLA_EPCO( 'B', EFORM, EQNX )
        FEPOCH = SLA_EPCO( 'B', EFORM, EPOCH )

*      Convert to B1950 FK4 without proper motion
        CALL SLA_FK54Z( INP(1), INP(2), FEPOCH, A, B, DA, DB )

*      Remove E-terms
        CALL SLA_SUBET( A, B, 1950D0, AW, BW )

*      Precess to final equinox
        CALL SLA_PRECES( 'FK4', 1950D0, FEQNX, AW, BW )

*      Add E-terms
        CALL sla_ADDET( AW, BW, FEPOCH, OUT(1), OUT(2) )

*    Ecliptic
      ELSE IF ( N3 .EQ. 'ECL' ) THEN

        CALL SLA_EQECL( INP(1), INP(2),
     :                  SLA_EPJ2D(SLA_EPCO( 'J', EFORM, EPOCH )),
     :                  OUT(1), OUT(2) )

*    Galactic
      ELSE IF ( N3 .EQ. 'GAL' ) THEN

        CALL SLA_EQGAL( INP(1), INP(2), OUT(1), OUT(2) )

*    Supergalactic
      ELSE IF ( N3 .EQ. 'SUP' ) THEN

        CALL SLA_EQGAL( INP(1), INP(2), AW, BW )
        CALL SLA_GALSUP( AW, BW, OUT(1), OUT(2) )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'WCI_CNZ2S', STATUS )

      END
