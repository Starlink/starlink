      SUBROUTINE HIST( STATUS )
*+
*  Name:
*     HIST

*  Purpose:
*     Retrieves and displays processing history of a dataset

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL HIST( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The history information contained in a dataset is retrieved and
*     displayed. The history information can be divided in two - a
*     history control structure which defines the original creation date
*     of the dataset, the history update mode, and the number of history
*     entries, and secondly an array of history records, each containing
*     a creation date, creation command and optional history text.

*  Usage:
*     HIST dataset [dev] [lines]

*  Environment Parameters:
*     INP = UNIV (Given)
*        The datatset whose processing history is to be displayed
*     DEV = CHARACTER (Given)
*        Output device
*     LINES = INTEGER (Given)
*        Maximum number of text lines to be displayed

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

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     HIST Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/hist.html

*  Keywords:
*     package:hist, usage:public, history, display

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     Jim Peden (JCMP,University of Birmingham)
*     Phil Andrews (ROSAT,University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      4 Jul 84 (JCMP):
*        Original version
*     27 Jan 86 V0.4-1 (JCMP):
*        ADAM version
*      5 Mar 86 V0.4-2 (GKS):
*        Handling of text component & output to LP
*     13 Nov 86 V0.4-3 (JKD):
*        Modified to ROSAT standard
*      7 Jan 87 V0.4-4 (RJV):
*        Max text lines increased to 200 and some general tidying
*      1 Aug 88 V1.0-0 (DJA):
*        Asterix88 version. General tidying.
*     11 Nov 88 V1.5-0 (PLA):
*        Altered to use HIST_CMN & new HIST_ routines
*     29 Oct 91 V1.5-1 (DJA):
*        Incorrect processing of LINES_OF_TEXT corrected
*     27 Mar 92 V1.6-0 (DJA):
*        Removed most VMS specifics
*     28 Apr 92 V1.6-1 (DJA):
*        Inserted missing CLOSE(LUN)
*     21 Sep 92 V1.6-2 (DJA):
*        Renamed to HIST to avoid csh command clash
*      7 Jun 93 V1.7-0 (DJA):
*        Report value of UPDATE_MODE
*      4 May 94 V1.7-1 (DJA):
*        Use AIO to perform output
*     24 Nov 94 V1.8-0 (DJA):
*        Now use USI for user interface
*     16 Mar 95 V1.8-1 (DJA):
*        Total re-write for ADI. No longer needs history common block.
*        Output style tidied up.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER               ( VERSION = 'HIST Version 1.8-1' )

*  Local Variables:
      CHARACTER*30		CDATE			! Creation date
      CHARACTER*80		CREATOR			! Creation command
      CHARACTER*30		HOST			! Machine of creation
      CHARACTER*132		FILE,PATH		! Path info
      CHARACTER*132           	STRING           	! String value

      INTEGER			CID			! Character cell
      INTEGER			HCID			! History control data
      INTEGER			HRID			! History record
      INTEGER			I			! Loop over records
      INTEGER			IFID			! Input dataset
      INTEGER			ILINE			! Loop over text lines
      INTEGER                   MAXLIN           	! Max # lines of text
							! user wants per record
      INTEGER			NLEV			! Path levels
      INTEGER			NLINE			! # lines of text
      INTEGER                 	OCH              	! Output channel
      INTEGER                 	OUTWIDTH         	! Required for subroutine, not used
      INTEGER			NREC          		! # history records
      INTEGER			SLEN			! Length of a string

      LOGICAL                 	OK               	! History file OK
      LOGICAL			THERE			! Object exists
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version
      CALL MSG_PRNT( VERSION )

*  Initialize ASTERIX subroutines
      CALL AST_INIT

*  Associate dataset
      CALL USI_TASSOCI( 'INP', '*', 'READ', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check there is history available
      CALL HSI_OK( IFID, OK, STATUS )
      IF ( ( STATUS .NE. SAI__OK ) .OR. .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'No valid history available', STATUS )
        GOTO 99
      END IF

*  Output destination
      CALL AIO_ASSOCO( 'DEV', 'LIST', OCH, OUTWIDTH, STATUS )

*  How much text information ?
      CALL USI_GET0I( 'LINES', MAXLIN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 90
      MAXLIN = MAX( 1, MAXLIN )

*  Print out 'header' info
      IF ( OUTWIDTH .EQ. 132 ) THEN
        CALL AIO_TITLE( OCH, VERSION, STATUS )
      END IF
      CALL AIO_BLNK( OCH, STATUS )
      CALL ADI_FTRACE( IFID, NLEV, PATH, FILE, STATUS )
      CALL MSG_SETC( 'FILE', FILE )
      CALL AIO_WRITE( OCH, 'History of  : ^FILE', STATUS )
      CALL MSG_SETC( 'PATH', PATH )
      CALL AIO_WRITE( OCH, '              ^PATH', STATUS )

*  Get control data
      CALL HSI_GETCTR( IFID, HCID, STATUS )

*  Date of creation
      CALL ADI_THERE( HCID, 'Date', THERE, STATUS )
      IF ( THERE ) THEN
        CALL ADI_CGET0C( HCID, 'Date', STRING, STATUS )
        CALL MSG_SETC( 'DATE', STRING )
        CALL AIO_WRITE( OCH, 'Created     : ^DATE', STATUS )
      END IF

*  Update mode
      CALL ADI_THERE( HCID, 'Verbosity', THERE, STATUS )
      IF ( THERE ) THEN
        CALL ADI_CGET0C( HCID, 'Verbosity', STRING, STATUS )
      ELSE
        STRING = 'NORMAL'
      END IF
      CALL MSG_SETC( 'MODE', STRING )
      CALL AIO_WRITE( OCH, 'Update mode : ^MODE', STATUS )

*  Number of records
      CALL ADI_CGET0I( HCID, 'NRECORD', NREC, STATUS )
      CALL MSG_SETI( 'NREC', NREC )
      CALL AIO_WRITE( OCH, 'Contains    : ^NREC records', STATUS )

*  Release the control info
      CALL ADI_ERASE( HCID, STATUS )

*  Loop over the records, dumping text
      DO I = 1, NREC

*    Annouce record
        CALL AIO_BLNK( OCH, STATUS )
        CALL MSG_SETI( 'REC', I )
        CALL AIO_WRITE( OCH, 'Record ^REC:', STATUS )
        CALL AIO_WRITE( OCH, ' ', STATUS )

*    Locate the I'th record
        CALL HSI_GETREC( IFID, I, HRID, STATUS )

*    Extract creation command, date and host name
        CALL ADI_CGET0C( HRID, 'Creator', CREATOR, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          CREATOR = 'Unknown'
        END IF
        CALL MSG_SETC( 'CREATOR', CREATOR )
        CALL AIO_IWRITE( OCH, 2, 'Creator: ^CREATOR', STATUS )
        CALL ADI_CGET0C( HRID, 'Date', CDATE, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          CDATE = 'Unknown'
        ELSE
          CALL CHR_LDBLK( CDATE )
        END IF
        CALL MSG_SETC( 'DATE', CDATE )
        CALL AIO_IWRITE( OCH, 2, 'Date:    ^DATE', STATUS )
        CALL ADI_CGET0C( HRID, 'Host', HOST, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          HOST = 'Unknown'
        END IF
        CALL MSG_SETC( 'HOST', HOST )
        CALL AIO_IWRITE( OCH, 2, 'Host:    ^HOST', STATUS )

*    Text exists?
        CALL ADI_THERE( HRID, 'Text', THERE, STATUS )
        IF ( THERE ) THEN

*      Number of text records
          CALL ADI_CSIZE( HRID, 'Text', NLINE, STATUS )

*      Loop over them and display 'em
          DO ILINE = 1, MIN(NLINE,MAXLIN)

*        Locate the string
            CALL ADI_CCELL( HRID, 'Text', 1, ILINE, CID, STATUS )
            CALL ADI_GET0C( CID, STRING, STATUS )

*        Print it if non-blank
            SLEN = CHR_LEN(STRING)
            IF ( SLEN .GT. 0 ) THEN
              IF ( ILINE .EQ. 1 ) THEN
                CALL AIO_IWRITE( OCH, 2, 'Text:    '//STRING(:SLEN),
     :                           STATUS )
              ELSE
                CALL AIO_IWRITE( OCH, 11, STRING(:SLEN), STATUS )
              END IF
            END IF

*        Release this cell
            CALL ADI_ERASE( CID, STATUS )

          END DO

        END IF

*    Release this record
        CALL ADI_ERASE( HRID, STATUS )

      END DO

*  Release the control data
      CALL ADI_ERASE( HCID, STATUS )

*  Clean up
 90   CALL AIO_CANCL( 'DEV', STATUS )

 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
