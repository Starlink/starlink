      SUBROUTINE ADI2_HPCMT( HDUID, CMNT, STATUS )
*+
*  Name:
*     ADI2_HPCMT

*  Purpose:
*     Write a comment to a specified HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_HPCMT( HDUID, CMNT, STATUS )

*  Description:
*     Write a comment string to an HDU

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of HDU cache object
*     CMNT = CHARACTER*(*)
*        The comment text. If the first character is '@' then the comment
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
      CHARACTER*(*)		CMNT

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL                  ADI2_MKIDX
        CHARACTER*8             ADI2_MKIDX

*  Local Variables:
      CHARACTER*8		NAME			! Cache object name

      INTEGER			CID			! Comment cache object
      INTEGER			COUNT			! Comment count
      INTEGER			FC			! 1st comment char
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First character of cmment
      IF ( CMNT(1:1) .EQ. '@' ) THEN
        FC = 2
      ELSE
        FC = 1
      END IF

*  Get current comment count
      CALL ADI_CGET0I( HDUID, 'CommentCount', COUNT, STATUS )

*  Increment count
      COUNT = COUNT + 1

*  Create name
      NAME = ADI2_MKIDX( 'COMM', COUNT )

*  Create cache object
      CALL ADI2_CFIND_CREC( HDUID, 'Crd', NAME, 'FITScommCache',
     :                      CID, STATUS )

*  Write the data
      CALL ADI_CPUT0C( CID, 'Value', CMNT(FC:), STATUS )

*  Release the cache object
      CALL ADI_ERASE( CID, STATUS )

*  Update the comment count
      CALL ADI_CPUT0I( HDUID, 'CommentCount', COUNT, STATUS )

*  Mark HDU as modified
      IF ( FC .EQ. 1 ) THEN
        CALL ADI_CPUT0L( HDUID, 'Modified', .TRUE., STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_HPCMT', STATUS )

      END
