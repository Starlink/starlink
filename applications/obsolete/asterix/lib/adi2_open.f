      SUBROUTINE ADI2_OPEN( FSPEC, MODE, ID, STATUS )
*+
*  Name:
*     ADI2_OPEN

*  Purpose:
*     Attempt to open an FITS file. If the file is opened ok then the
*     FileHandle object pointed to by ID is updated.

*  Language:
*     Fortran

*  Invocation:
*     CALL ADI2_OPEN( FSPEC, MODE, ID, STATUS )

*  Description:
*     Attempts to open the named file object as an HDS file. If successful
*     the routine stores the logical unit and HDU number on the property
*     list of the ID object.

*  Arguments:
*     FSPEC = CHAR (Given)
*        Name of the object on which HDS access to be attempted
*     MODE = CHAR (Given)
*        File access mode
*     ID = INTEGER (Given)
*        ADI identifier of FileHandle object
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

*  References:
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (JET-X,University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     15 Jul 1994 (DJA):
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
      CHARACTER*(*)		FSPEC
      CHARACTER*(*)		MODE
      INTEGER			ID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*132		ERRTEXT			! FITS error text
      CHARACTER*20		HDUTYPE			! HDU type
      CHARACTER*80		KEYWRD			! Keyword name

      INTEGER			BSIZE			! FITS block size
      INTEGER			FITSTAT			! FITS inherited status
      INTEGER			HDU			! HDU number
      INTEGER			IMODE			! FITSIO mode
      INTEGER			LFILEC			! Last char in filename
      INTEGER			LUN			! Logical unit number
*.

*    Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Parse the file name
      CALL ADI2_PARSE( FSPEC, LFILEC, HDU, KEYWRD, STATUS )

*    Get a logical unit from the system
      CALL FIO_GUNIT( LUN, STATUS )

*    Allocated ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Parse access mode
        IF ( (MODE(1:1) .EQ. 'R') .OR. (MODE(1:1) .EQ. 'r') ) THEN
          IMODE = 0
        ELSE IF ( (MODE(1:1) .EQ. 'U') .OR. (MODE(1:1) .EQ. 'u') ) THEN
          IMODE = 1
        END IF

*      Try to open file
        FITSTAT = 0
        CALL FTOPEN( LUN, FSPEC(:LFILEC), MODE, BSIZE, FITSTAT )

*      Opened ok?
        IF ( FITSTAT .EQ. 0 ) THEN

*        Skip to HDU if specified
          IF ( HDU .GT. 0 ) THEN
            CALL FTMRHD( LUN, HDU, HDUTYPE, FITSTAT )
            IF ( FITSTAT .NE. 0 ) THEN
              STATUS = SAI__ERROR
              CALL MSG_SETI( 'HDU', HDU )
              CALL ERR_REP( ' ', 'Unable to move to HDU ^HDU', STATUS )
              CALL FTGERR( FITSTAT, ERRTEXT )
              CALL MSG_SETC( 'REASON', ERRTEXT )
              CALL ERR_REP( ' ', '^REASON', STATUS )
            END IF

          END IF

*        Write HDU number
          CALL ADI_CPUT0I( ID, '.HDU', HDU, STATUS )

*        Write extra info into the file handle object
          CALL ADI_CPUT0I( ID, '.LUN', LUN, STATUS )
          CALL ADI_CPUT0I( ID, '.BLOCK_SIZE', BSIZE, STATUS )
          CALL ADI_CPUT0I( ID, '.NHDU', 1, STATUS )

*      Failed to open
        ELSE
          CALL FIO_PUNIT( LUN, STATUS )

          STATUS = SAI__ERROR
          CALL FTGERR( FITSTAT, ERRTEXT )
          CALL MSG_SETC( 'REASON', ERRTEXT )
          CALL ERR_REP( ' ', '^REASON', STATUS )

*      End opened ok test
        END IF

*    End acquired logical unit test
      END IF

      END
