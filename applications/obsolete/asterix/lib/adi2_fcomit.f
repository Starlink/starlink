      SUBROUTINE ADI2_FCOMIT( FID, STATUS )
*+
*  Name:
*     ADI2_FCOMIT

*  Purpose:
*     Commit buffer changes to a FITSfile object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_FCOMIT( FID, STATUS )

*  Description:
*     Commit any changes to keywords or data to the FITS file on disk. The
*     file is not closed.

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of the FITSfile object
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
*     2 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			FID			! FITSfile identifier

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		HNAME			! HDU name

      INTEGER			HDUID			! HDU identifier
      INTEGER			HIID			! HDU index
      INTEGER			IHDU			! HDU loop variable
      INTEGER			NHDU			! HDU count

      LOGICAL			CHANGED			! HDU is updated?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get number of HDU's
      CALL ADI_FIND( FID, 'HduIndex', HIID, STATUS )
      CALL ADI_NCMP( HIID, NHDU, STATUS )
      CALL ADI_ERASE( HIID, STATUS )

*  Loop over them
      DO IHDU = 1, NHDU

*    Get value of HDU index
        CALL ADI2_GETHDI( FID, IHDU, HNAME, STATUS )

*    Locate the HDU
        CALL ADI2_FNDHDU( FID, HNAME, HDUID, STATUS )

*    Has it been updated?
        CALL ADI_CGET0L( HDUID, 'Changed', CHANGED, STATUS )
        IF ( CHANGED ) THEN

*      Update file for this HDU
          CALL ADI2_FCOMIT_HDU( HDUID, STATUS )

        END IF
        CALL ADI_CGET0L( HDUID, 'DataChanged', CHANGED, STATUS )
        IF ( CHANGED ) THEN
          print *,'There are uncommitted data changes in ',HNAME
        END IF

*    Release this HDU
        CALL ADI_ERASE( HDUID, STATUS )

      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_FCOMIT', STATUS )

      END


      SUBROUTINE ADI2_FCOMIT_HDU( HDUID, STATUS )
*+
*  Name:
*     ADI2_FCOMIT_HDU

*  Purpose:
*     Commit buffer changes to a FITShdu object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_FCOMIT_HDU( HDUID, STATUS )

*  Description:
*     Commit any changes to keywords or data to the FITS file on disk. The
*     file is not closed.

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of the FITShdu object
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
*     2 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			HDUID			! HDU identifier

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*1		CFORM			! Card form, K,C or H
      CHARACTER*70		CMT			! Keyword comment
      CHARACTER*20		CLASS			! Keyword value class
      CHARACTER*132		CVALUE			! Keyword value
      CHARACTER*8		KEYWRD			! Keyword name

      DOUBLE PRECISION		DVALUE			! Keyword value

      REAL			RVALUE			!

      INTEGER			CIID			! Card index object
      INTEGER			FCARD			! First card to update
      INTEGER			FSTAT			! FITSIO status
      INTEGER			ICARD			! Loop over HDU cards
      INTEGER			IVALUE			! Keyword value
      INTEGER			LUN			! Logical unit number
      INTEGER			NCARD			! # cards in HDU
      INTEGER			OBJID			! Card data

      LOGICAL			CHANGED			! Card data updated?
      LOGICAL			LVALUE			! Logical keyword value
      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract logical unit
      CALL ADI2_HDULUN( HDUID, LUN, STATUS )

*  Get number of HDU cards and update threshold
      CALL ADI_CGET0I( HDUID, 'Ncard', NCARD, STATUS )
      CALL ADI_CGET0I( HDUID, 'MinDiffCard', FCARD, STATUS )

*  HDU cards updated?
      IF ( FCARD .GT. 0 ) THEN

*    Locate card index
        CALL ADI_FIND( HDUID, 'CardIndex', CIID, STATUS )

*    Loop over cards writing them to disk
        FSTAT = 0
        DO ICARD = FCARD, NCARD

*      Get card index entry
          CALL ADI2_GETCIE( HDUID, CIID, ICARD, CFORM, OBJID, STATUS )

*      Has the object changed?
          CALL ADI_CGET0L( OBJID, '.Changed', CHANGED, STATUS )
          IF ( CHANGED ) THEN

*        Keyword?
            IF ( CFORM .EQ. 'K' ) THEN

*          Get keyword name
              CALL ADI_NAME( OBJID, KEYWRD, STATUS )

*          Get keyword comment
              CALL ADI_THERE( OBJID, '.Comment', THERE, STATUS )
              IF ( THERE ) THEN
                CALL ADI_CGET0C( OBJID, '.Comment', CMT, STATUS )
              ELSE
                CALL ADI2_STDCMT( KEYWRD, CMT, STATUS )
              END IF

*          Write keyword data
              CALL ADI_TYPE( OBJID, CLASS, STATUS )
              IF ( CLASS(1:1) .EQ. 'D' ) THEN
                CALL ADI_GET0D( OBJID, DVALUE, STATUS )
                CALL FTPKYG( LUN, KEYWRD, DVALUE, 8, CMT, FSTAT )
              ELSE IF ( CLASS(1:1) .EQ. 'R' ) THEN
                CALL ADI_GET0R( OBJID, RVALUE, STATUS )
                CALL FTPKYE( LUN, KEYWRD, RVALUE, 8, CMT, FSTAT )
              ELSE IF ( CLASS(1:1) .EQ. 'I' ) THEN
                CALL ADI_GET0I( OBJID, IVALUE, STATUS )
                CALL FTPKYJ( LUN, KEYWRD, IVALUE, CMT, FSTAT )
              ELSE IF ( CLASS(1:1) .EQ. 'L' ) THEN
                CALL ADI_GET0L( OBJID, LVALUE, STATUS )
                CALL FTPKYL( LUN, KEYWRD, LVALUE, CMT, FSTAT )
              ELSE
                CALL ADI_GET0C( OBJID, CVALUE, STATUS )
                CALL FTPKYS( LUN, KEYWRD, CVALUE, CMT, FSTAT )
              END IF

*        Comment?
            ELSE IF ( CFORM .EQ. 'C' ) THEN
              CALL ADI_GET0C( OBJID, CMT, STATUS )
              CALL FTPCOM( LUN, CMT, FSTAT )
              KEYWRD = 'COMMENT'

*        History?
            ELSE IF ( CFORM .EQ. 'H' ) THEN
              CALL ADI_GET0C( OBJID, CMT, STATUS )
              CALL FTPHIS( LUN, CMT, FSTAT )
              KEYWRD = 'HISTORY'

            END IF

*        Error writing?
            IF ( FSTAT .NE. 0 ) THEN
              CALL MSG_SETC( 'KEY', KEYWRD )
              CALL ADI2_FITERP( FSTAT, STATUS )
              CALL ERR_REP( ' ', 'Error comitting keyword ^KEY to disk',
     :                    STATUS )
              GOTO 99
            END IF

*        Mark object as not changed
            CALL ADI_CPUT0L( OBJID, '.Changed', .FALSE., STATUS )

*      End of object changed test
          END IF

*      Release this object
          CALL ADI_ERASE( OBJID, STATUS )

        END DO

*    Release card index
        CALL ADI_ERASE( CIID, STATUS )

      END IF

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_FCOMIT_HDU', STATUS )
      END IF

      END
