      SUBROUTINE ADI2_FTRACE( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     ADI2_FTRACE

*  Purpose:
*     Return file trace information about an FITSfile based object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_FTRACE( NARG, ARGS, OARG, STATUS )

*  Description:
*     {routine_description}

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
*     20 Mar 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! Standard SAE constants

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*200		FILE			! HDS trace info

      INTEGER			FSTAT			! I/o status code
      INTEGER			FPID			! Fpath value id
      INTEGER			IHDU			! HDU number
      INTEGER			LUN			! Logical unit number
      INTEGER			NLEV			! # levels
      INTEGER			PID			! Copy of Fpath

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract LOGICAL UNIT
      CALL ADI2_GETLUN( ARGS(1), LUN, STATUS )

*  Filename from logical unit
      INQUIRE( UNIT=LUN, NAME=FILE, IOSTAT=FSTAT )

*  Report error if that failed
      IF ( FSTAT .NE. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unable to get file name for FITSfile'/
     :                /' object', STATUS )

      ELSE

*    Create structure and store filename
        CALL ADI_NEW0( 'STRUC', OARG, STATUS )
        CALL ADI_CPUT0C( OARG, 'File', FILE(:CHR_LEN(FILE)), STATUS )

*    The path stored in the Fpath member
        CALL ADI_THERE( ARGS(1), 'Fpath', THERE, STATUS )
        IF ( THERE ) THEN
          CALL ADI_FIND( ARGS(1), 'Fpath', FPID, STATUS )
          CALL ADI_COPY( FPID, PID, STATUS )
          CALL ADI_CPUTID( OARG, 'Path', PID, STATUS )
          CALL ADI_ERASE( FPID, STATUS )
        ELSE
          CALL ADI_CPUT0C( OARG, 'Path', ' ', STATUS )
          NLEV = 1
        END IF
        CALL ADI_CPUT0I( OARG, 'Nlev', NLEV, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_FTRACE', STATUS )

      END
