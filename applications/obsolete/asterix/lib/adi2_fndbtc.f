      SUBROUTINE ADI2_FNDBTC( HDUID, COLUMN, ICOL, STATUS )
*+
*  Name:
*     ADI2_FNDBTC

*  Purpose:
*     Locate a column number given its name

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_FNDBTC( HDUID, COLUMN, ICOL, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of FITShdu object
*     COLUMN = CHARACTER*(*) (given)
*        The name of the column to searchg for
*     ICOL = INTEGER (returned)
*        The number of the column
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 Sep 1995 (DJA):
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
      INTEGER			HDUID
      CHARACTER*(*)		COLUMN

*  Arguments Returned:
      INTEGER			ICOL

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		COLNAM			! Column name
      CHARACTER*8		KEYWRD			! Keyword name
      CHARACTER*72		CMNT			! (rb)

      INTEGER			IFLD			! Loop over columns
      INTEGER			NFLD			! # columns in table
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      ICOL = 0

*  Get number of fields
      CALL ADI2_HGKYI( HDUID, 'TFIELDS', NFLD, CMNT, STATUS )
      IFLD = 1
      DO WHILE ( (IFLD.LE.NFLD) .AND. (ICOL.EQ.0)
     :           .AND. (STATUS.EQ.SAI__OK) )

*    Get value of column name
        CALL ADI2_HGKYIC( HDUID, 'TTYPE', IFLD, COLNAM, CMNT, STATUS )

*    Does it match?
        IF ( (STATUS.EQ.SAI__OK) .AND. (COLNAM.EQ.COLUMN) ) THEN
          ICOL = IFLD
        ELSE IF ( STATUS .NE. SAI__OK ) THEN
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'KEY', KEYWRD )
          CALL ERR_REP( ' ', 'Badly formed binary table, ^KEY is '/
     :                                         /'missing', STATUS )
        ELSE
          IFLD = IFLD + 1
        END IF

      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_FNDBTC', STATUS )

      END
