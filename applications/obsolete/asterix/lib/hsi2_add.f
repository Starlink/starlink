      SUBROUTINE HSI2_ADD( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     HSI2_ADD

*  Purpose:
*     Create a new history record in an FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI2_ADD( NARG, ARGS, OARG, STATUS )

*  Description:
*     Creates a new history record in an FITS file.

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:

*  Implementation Deficiencies:
*     The constants in this routine should be loadable resources. This
*     would enable the user to turn off all history creation (somehow).

*  References:
*     HSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/hsi.html

*  Keywords:
*     package:hsi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Feb 1995 (DJA):
*        Original version.
*     13 May 1997 (RB):
*        Update the CREATOR keyword each time.
*     25 Nov 1997 (RB):
*        Add lines to a special HISTORY extension.
*     10 Feb 1998 (RB):
*        Converted to new linked structure format.
*     20 May 1998 (RB):
*        Write out the USER
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'ADI_PAR'					! ADI constants
      INCLUDE 'HSI_PAR'

*  Arguments Given:
      INTEGER			NARG, ARGS(*)

*  Arguments Returned:
      INTEGER			OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      CHARACTER*8		UMODE
        PARAMETER		( UMODE = 'NORMAL' )

*  Local Variables:
      CHARACTER*80		CMNT			! FITS comment line
      CHARACTER*4		RSTR			! Record number as string

      CHARACTER*80		NAME			! Creator name
      CHARACTER*12           	SYSNAME, NODENAME, USERNAME,
     :                       	RELEASE, VERSION, MACHINE
      CHARACTER*18		TSTR			! Time string

      INTEGER			IVERB			! Verbosity
      INTEGER			PHDU			! Main HDU in file
      INTEGER			HHDU			! HISTORY HDU in file
      INTEGER			CURREC			! Currect history record
      INTEGER			NDIG			! Number of digits
      INTEGER			RHDU			! New record HDU in HISTORY

      LOGICAL			DIDCRE			! Was the HDU created?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No output from this method
      OARG = ADI__NULLID

*  Locate our primary and HISTORY HDUs
      CALL ADI2_FNDHDU( ARGS(1), ' ', .FALSE., PHDU, STATUS )
      CALL ADI2_FNDHDU( ARGS(1), 'HISTORY', .FALSE., HHDU, STATUS )
      IF ( HHDU .EQ. ADI__NULLID ) THEN
        CALL ERR_ANNUL( STATUS )
	CALL HSI_NEW( ARGS(1), STATUS )
        CALL ADI2_FNDHDU( ARGS(1), 'HISTORY', .FALSE., HHDU, STATUS )
      END IF

*  Get verbosity
      CALL HSI2_GETVRB( HHDU, IVERB, STATUS )

*  Only write text if verbosity is QUIET or greater
      IF ( IVERB .GE. HSI__QUIET ) THEN

*  Get username
      CALL PSX_GETENV( 'USER', USERNAME, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        USERNAME = 'unknown'
      END IF

*    Get system stuff including node name
        CALL PSX_UNAME( SYSNAME, NODENAME, RELEASE, VERSION, MACHINE,
     :                  STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          NODENAME = 'unknown'
        END IF

*    Get date and time
        CALL TCI_CTIME( TSTR, STATUS )

*    Get the command name
        CALL ADI_GET0C( ARGS(2), NAME, STATUS )

*    Update the value of the CREATOR keyword (or create if first time)
        CALL ADI2_HPKYC( PHDU, 'CREATOR', NAME,
     :                   'Creator of this file', STATUS )

*    Get current record number
        CALL ADI2_HGKYI( HHDU, 'CHILDREN', CURREC, CMNT, STATUS )
        CURREC = CURREC + 1
        CALL ADI2_HPKYI( HHDU, 'CHILDREN', CURREC, CMNT, STATUS )
        CALL CHR_ITOC( CURREC, RSTR, NDIG )
        CALL ADI2_HPKYC( HHDU, 'CHILD'//RSTR(:NDIG),
     :                   'RECORD'//RSTR(:NDIG),
     :                   'Name of child extension',
     :                   STATUS )

*    Write hew history record extension
        CALL ADI2_CFIND( ARGS(1), 'RECORD'//RSTR(:NDIG), ' ', ' ',
     :                   .TRUE., .FALSE., ' ', 0, 0, DIDCRE, RHDU,
     :                   STATUS )
        CALL ADI2_HPKYC( RHDU, 'EXTNAME', 'RECORD'//RSTR(:NDIG),
     :                   'Contains ASTERIX HIST_REC structure', STATUS )
        CALL ADI2_HPKYC( RHDU, 'TYPE', 'HIST_REC',
     :                   'Data type of this extension', STATUS )
        CALL ADI2_HPKYC( RHDU, 'USER', USERNAME,
     :                   'Username of person modifying file', STATUS )
        CALL ADI2_HPKYC( RHDU, 'HOST', NODENAME,
     :                   'Machine application was run from', STATUS )
        CALL ADI2_HPKYC( RHDU, 'DATE', TSTR,
     :                   'Date & time application was started', STATUS )
        CALL ADI2_HPKYC( RHDU, 'COMM', NAME,
     :                   'Version of application being run', STATUS )
        CALL ADI2_HPKYI( RHDU, 'TEXT_REC', 0,
     :                   'Current text line number', STATUS )
        CALL ADI2_HPKYC( RHDU, 'PARENT', 'HISTORY',
     :                   'Parent HDU of this extension', STATUS )
        CALL ADI2_HPKYI( RHDU, 'CHILDREN', 0,
     :                   'Number of children for this HDU', STATUS )

*  End of verbosity test
      END IF

*  Release the HDU
      CALL ADI_ERASE( PHDU, STATUS )

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'HSI2_ADD', STATUS )
      END IF

      END
