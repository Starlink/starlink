      SUBROUTINE ADI2_BTCTYP( HDUID, BCOL, DIMS, TYP, STATUS )
*+
*  Name:
*     ADI2_BTCTYP

*  Purpose:
*     Convert binary table column TTYPE keyword to DIMS and TYP

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_BTCTYP( HDUID, BCOL, DIMS, TYP, STATUS )

*  Description:
*     Convert binary table column TTYPE keyword to DIMS and TYP

*  Arguments:
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
*     EDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/edi.html

*  Keywords:
*     package:edi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER                   HDUID, BCOL

*  Arguments Returned:
      INTEGER			DIMS, TYP

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      CHARACTER*6		TSTRING
        PARAMETER               ( TSTRING = 'BIJEDC' )

*  Local Variables:
      CHARACTER*20		TYPE			! Column type

      INTEGER			IC			! Character ptr
      INTEGER			TPOS			! Type code index
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read table item describing the type of the data stored
      CALL ADI2_HGKYIC( HDUID, 'TFORM', BCOL, TYPE, STATUS )

*  Translate type string to dimensions and ADI type. The format is
*  [<dims>]<tcode> where <dims> is some number and <tcode> is one of
*  TSTRING
      TPOS = INDEX(TSTRING,TYPE(1:1))
      IF ( TPOS .EQ. 0 ) THEN
        IC = 2
        DO WHILE ( TPOS .EQ. 0 )
          TPOS = INDEX(TSTRING,TYPE(IC:IC))
          IF ( TPOS .EQ. 0 ) IC = IC + 1
        END DO
        CALL CHR_CTOI( TYPE(1:IC-1), DIMS, STATUS )
      END IF
      IF ( TPOS .EQ. 1 ) THEN
        TYP = 'BYTE'
      ELSE IF ( TPOS .EQ. 2 ) THEN
        TYP = 'WORD'
      ELSE IF ( TPOS .EQ. 3 ) THEN
        TYP = 'INTEGER'
      ELSE IF ( TPOS .EQ. 4 ) THEN
        TYP = 'REAL'
      ELSE IF ( TPOS .EQ. 5 ) THEN
        TYP = 'DOUBLE'
      ELSE IF ( TPOS .EQ. 6 ) THEN
        TYP = 'CHAR'
      END IF

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_BTCTYP', STATUS )

      END
