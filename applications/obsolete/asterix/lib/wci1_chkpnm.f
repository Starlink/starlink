      SUBROUTINE WCI1_CHKPNM( NAME, STATUS )
*+
*  Name:
*     WCI1_CHKPNM

*  Purpose:
*     Check a WCI map projection name

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI1_CHKPNM( NAME, STATUS )

*  Description:
*     Tests the candidate map projection name NAME against the allowed
*     names. Only the first 3 characters of the name are considered. If the
*     name is invalid then an error is reported. This is an internal routine
*     for use within WCI.

*  Arguments:
*     NAME = CHARACTER*(*) (given)
*        Name of the map projection
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
*     WCI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/wci.html

*  Keywords:
*     package:wci, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     4 Jan 1995 (DJA):
*        Original version.
*     {enter_changes_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(*)		NAME			! Candidate name

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET

*  Local Variables:
      INTEGER			LC			! Significant length
							! of NAME
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Significant length of NAME. Only look at first 3 characters
      LC = MAX( LEN(NAME), 3 )

*  Test against allowed alternatives
      IF ( .NOT. CHR_INSET( 'AZP,TAN,SIN,STG,ARC,ZPN,ZEA,AIR,CYP,CAR,'/
     :                     /'MER,CEA,COP,COD,COE,COO,BON,PCO,GLS,PAR,'/
     :                     /'AIT,MOL,CSC,QSC,TSC', NAME(:LC) ) ) THEN
        CALL MSG_SETC( 'NAME', NAME )
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'The name /^NAME/ is not a known '/
     :                /'map projection', STATUS )
      END IF

      END
