      SUBROUTINE ADI2_ADDKEY( HDUID, KEY, KID, UPDATE, STATUS )
*+
*  Name:
*     ADI2_ADDKEY

*  Purpose:
*     Add a keyword description to an HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_ADDKEY( HDUID, KEY, KID, UPDATE, STATUS )

*  Description:
*     Write value of keyword to specified HDU. Any existing keyword value
*     is overwritten.

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of HDU object
*     KEY = CHARACTER*(*) (given)
*        The name of the keyword to be extracted
*     KID = INTEGER (given)
*        ADI object describing keyword value
*     UPDATE = LOGICAL (given)
*        The keyword is being added with the intention of updating the file?
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
      INTEGER			HDUID, KID
      CHARACTER*(*)		KEY
      LOGICAL			UPDATE

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			KCID			! Keywords list
      INTEGER			MCARD			! First changed card
      INTEGER			NCARD			! HDU card number
      INTEGER			OKID			! Existing keyword data

      LOGICAL			THERE			! Keyword exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate keywords container
      CALL ADI_FIND( HDUID, 'Keys', KCID, STATUS )

*  Does our keyword exist?
      IF ( UPDATE ) THEN
        CALL ADI_THERE( KCID, KEY, THERE, STATUS )
        IF ( THERE ) THEN

*      Locate the keyword
          CALL ADI_FIND( KCID, KEY, OKID, STATUS )

*      Read the old keyword index
          CALL ADI_CGET0I( OKID, '.Icard', NCARD, STATUS )

*      Erase value
          CALL ADI_ERASE( OKID, STATUS )
          CALL ADI_CERASE( KCID, KEY, STATUS )

        END IF
      END IF

*  Mark keyword and HDU as changed
      CALL ADI_CPUT0L( KID, '.Changed', UPDATE, STATUS )
      IF ( UPDATE ) THEN
        CALL ADI_CPUT0L( HDUID, 'Changed', .TRUE., STATUS )
      END IF

*  Get keyword number and update
      IF ( UPDATE ) THEN
        IF ( THERE ) THEN
          CALL ADI_CPUT0I( KID, '.Icard', NCARD, STATUS )
        ELSE
          CALL ADI2_ADDCRC( HDUID, 'K'//KEY, KID, NCARD, STATUS )
        END IF

*  Update first changed card number
        CALL ADI_CGET0I( HDUID, 'MinDiffCard', MCARD, STATUS )
        IF ( MCARD .EQ. 0 ) MCARD = NCARD
        CALL ADI_CPUT0I( HDUID, 'MinDiffCard', MIN(MCARD,NCARD),
     :                   STATUS )

      ELSE
        CALL ADI2_ADDCRC( HDUID, 'K'//KEY, KID, NCARD, STATUS )
      END IF

*  Write component to container
      CALL ADI_CPUTID( KCID, KEY, KID, STATUS )

*  Release keyword container
      CALL ADI_ERASE( KCID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_ADDKEY', STATUS )

      END
