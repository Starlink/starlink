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
*     {enter_new_authors_here}

*  History:
*     14 Feb 1995 (DJA):
*        Original version.
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

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Constants:
      CHARACTER*8		UMODE
        PARAMETER		( UMODE = 'NORMAL' )

*  Local Variables:
      CHARACTER*40		CREATOR			! Creator of file
      CHARACTER*80		HTXT			! History text line

      CHARACTER*80		NAME			! Creator name
      CHARACTER*12           	SYSNAME, NODENAME,
     :                       	RELEASE, VERSION, MACHINE
      CHARACTER*18		TSTR			! Time string

      INTEGER			IVERB			! Verbosity
      INTEGER			PHDU			! Main HDU in file
      INTEGER			TLEN			! Text length
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No output from this method
      OARG = ADI__NULLID

*  Locate our primary HDU
      CALL ADI2_FNDHDU( ARGS(1), ' ', PHDU, STATUS )

*  Get verbosity
      IVERB = HSI__QUIET
C      CALL HSI1_GETVRB( HLOC, IVERB, STATUS )

*  Only write text if verbosity is QUIET or greater
      IF ( IVERB .GE. HSI__QUIET ) THEN

*    Get system stuff including node name
        CALL PSX_UNAME( SYSNAME, NODENAME, RELEASE, VERSION, MACHINE,
     :                                                     STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          NODENAME = 'unknown'
        END IF

*    Get date and time
        CALL TCI_CTIME( TSTR, STATUS )

*    Get the command name
        CALL ADI_GET0C( ARGS(2), NAME, STATUS )

*    Format history line
        CALL MSG_SETC( 'CMD', NAME )
        CALL MSG_SETC( 'HOST', NODENAME )
        CALL MSG_SETC( 'TIME', TSTR )
        CALL MSG_MAKE( 'ASTHIS: ^CMD on ^HOST at ^TIME', HTXT, TLEN )

*    Write hew history record
        CALL ADI2_ADDHIS( PHDU, HTXT(:TLEN), .TRUE., STATUS )

*    Get the value of the CREATOR keyword. Only write this keyword if its
*    value is not currently defined
        CALL ADI2_HGKYC( PHDU, 'CREATOR', CREATOR, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          CALL ADI2_HPKYC( PHDU, 'CREATOR', NAME,
     :                     'Creator of this file', STATUS )
        END IF

*  End of verbosity test
      END IF

*  Release the HDU
      CALL ADI_ERASE( PHDU, STATUS )

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'HSI2_ADD', STATUS )
      END IF

      END
