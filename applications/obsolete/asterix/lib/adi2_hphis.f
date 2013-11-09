      SUBROUTINE ADI2_HPHIS( HDUID, HTXT, STATUS )
*+
*  Name:
*     ADI2_HPHIS

*  Purpose:
*     Write a history line to a specified HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_HPHIS( HDUID, HTXT, STATUS )

*  Description:
*     Write a history string to an HDU

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of HDU cache object
*     HTXT = CHARACTER*(*)
*        The history text. If the first character is '@' then the text
*        addition is an internal write and the HDU modified flag is not set
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
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 Sep 1995 (DJA):
*        Original version.
*     30 Apr 1997 (RB):
*        Make sure the card is not marked as written.
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
      CHARACTER*(*)		HTXT

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL                  ADI2_MKIDX
        CHARACTER*8             ADI2_MKIDX

*  Local Variables:
      CHARACTER*8		NAME			! Cache object name

      INTEGER			CID			! History cache object
      INTEGER			COUNT			! History count
      INTEGER			FC			! 1st history char
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First character of history
      IF ( HTXT(1:1) .EQ. '@' ) THEN
        FC = 2
      ELSE
        FC = 1
      END IF

*  Get current history count
      CALL ADI_CGET0I( HDUID, 'HistoryCount', COUNT, STATUS )

*  Increment count
      COUNT = COUNT + 1

*  Create name
      NAME = ADI2_MKIDX( 'HIST', COUNT )

*  Create cache object
      CALL ADI2_CFIND_CREC( HDUID, 'Crd', NAME, 'FITShistCache',
     :                      CID, STATUS )

*  Write the data
      CALL ADI_CPUT0C( CID, 'Value', HTXT(FC:), STATUS )
      CALL ADI_CPUT0L( CID, 'Written', .FALSE., STATUS )

*  Release the cache object
      CALL ADI_ERASE( CID, STATUS )

*  Update the history count
      CALL ADI_CPUT0I( HDUID, 'HistoryCount', COUNT, STATUS )

*  Mark HDU as modified
      IF ( FC .EQ. 1 ) THEN
        CALL ADI_CPUT0L( HDUID, 'Modified', .TRUE., STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_HPHIS', STATUS )

      END
