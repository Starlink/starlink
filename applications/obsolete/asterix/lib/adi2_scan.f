      SUBROUTINE ADI2_SCAN( HDUID, STATUS )
*+
*  Name:
*     ADI2_SCAN

*  Purpose:
*     Read keywords from an HDU and store

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_SCAN( HDUID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of FITShdu object
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

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*72		CMNT			! Comment
      CHARACTER*8		KEYWRD			! Keyword name
      CHARACTER*50		VALUE			! Keyword value

      DOUBLE PRECISION		DVAL			! Numeric key value

      INTEGER			FID			! File identifier
      INTEGER			FSTAT			! FITSIO status code
      INTEGER			HDUTYP			! HDU type
      INTEGER			IHDU			! HDU number
      INTEGER			IKEY			! Loop over keywords
      INTEGER			IVAL			! Integer key value
      INTEGER			LUN			! Logical unit number
      INTEGER			VID			! Value identifier

      LOGICAL			MORE			! More keywords?
      LOGICAL			UPDATE			! Update mode on HDU?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate file and extract logical unit
      CALL ADI_CGETREF( HDUID, '.File', FID, STATUS )
      CALL ADI2_GETLUN( FID, LUN, STATUS )

*  Get the HDU number
      CALL ADI_CGET0I( HDUID, 'Ihdu', IHDU, STATUS )

*  Move to the HDU
      CALL ADI2_MVAHDU( FID, LUN, IHDU, HDUTYP, STATUS )

*  Initialise
      IKEY = 1
      FSTAT = 0
      MORE = .TRUE.
      DO WHILE ( MORE .AND. (FSTAT .EQ. 0) )

*    Extract name and value
        CALL FTGKYN( LUN, IKEY, KEYWRD, VALUE, CMNT, FSTAT )

*    Ok?
        IF ( KEYWRD .EQ. 'END' ) THEN
          MORE = .FALSE.

*    Comment card?
        ELSE IF ( KEYWRD .EQ. 'COMMENT' ) THEN

*      Write comment to container
          CALL ADI2_ADDCMT( HDUID, CMNT, .FALSE., STATUS )

*      Next keyword
          IKEY = IKEY + 1

*    History card?
        ELSE IF ( KEYWRD .EQ. 'HISTORY' ) THEN

*      Write history to container
          CALL ADI2_ADDHIS( HDUID, CMNT, .FALSE., STATUS )

*      Next keyword
          IKEY = IKEY + 1

*    Good keyword record
        ELSE IF ( (FSTAT .EQ. 0) .AND. (KEYWRD.GT.' ') ) THEN

*      Create keyword object
          IF ( VALUE(1:1) .EQ. '''' ) THEN
            CALL ADI_NEWV0C( VALUE(2:CHR_LEN(VALUE)-1), VID, STATUS )
          ELSE
            CALL CHR_LDBLK( VALUE )
            IF ( INDEX( VALUE, '.' ) .GT. 0 ) THEN
              CALL CHR_CTOD( VALUE, DVAL, STATUS )
              CALL ADI_NEWV0D( DVAL, VID, STATUS )
            ELSE
              IF ( (VALUE(1:1) .EQ. 'T') .OR. (VALUE(1:1).EQ.'F') ) THEN
                CALL ADI_NEWV0L( (VALUE(1:1) .EQ. 'T'), VID, STATUS )
              ELSE
                CALL CHR_CTOI( VALUE, IVAL, STATUS )
                CALL ADI_NEWV0I( IVAL, VID, STATUS )
              END IF
            END IF

          END IF

*      Write comment if non-blank
          IF ( CMNT .GT. ' ' ) THEN
            CALL ADI_CNEWV0C( VID, '.Comment', CMNT(:CHR_LEN(CMNT)),
     :                        STATUS )
          END IF

*      Write keyword to container
          CALL ADI2_ADDKEY( HDUID, KEYWRD, VID, .FALSE., STATUS )

*      Next keyword
          IKEY = IKEY + 1

        ELSE
          MORE = .FALSE.

        END IF

      END DO

*  Warn if not END keyword
      IF ( MORE ) THEN
        CALL ADI_CGET0L( HDUID, 'Changed', UPDATE, STATUS )
        IF ( UPDATE ) THEN
          CALL ERR_ANNUL( STATUS )
        ELSE
          CALL ADI2_FITERP( FSTAT, STATUS )
          CALL ERR_REP( ' ', 'Unable to locate END keyword at '/
     :                  /'end of HDU', STATUS )
        END IF

      END IF

*  Mark as scanned
      CALL ADI_CPUT0L( HDUID, 'Scanned', .TRUE., STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_SCAN', STATUS )

      END
