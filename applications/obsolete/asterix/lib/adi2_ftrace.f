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
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     20 Mar 1995 (DJA):
*        Original version.
*      3 Mar 1997 (RB):
*        Get the full path name properly.
*      8 May 1997 (RB):
*        Put in a proper PATH value.
*     25 Jun 1997 (RB):
*        Trim out any remaining relative FILE bits.
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
      CHARACTER*200		FILE			! File name
      CHARACTER*200		PWD			! Current directory
      CHARACTER*200		PATH			! Object name

      INTEGER			FSTAT			! I/o status code
      INTEGER			LUN			! Logical unit number
      INTEGER			I, J			! String positions
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract LOGICAL UNIT
      CALL ADI2_GETLUN( ARGS(1), LUN, STATUS )

*  Filename from logical unit
      INQUIRE( UNIT=LUN, NAME=FILE, IOSTAT=FSTAT )

*  Now do it properly
      CALL PSX_GETENV( 'PWD', PWD, STATUS )

*  Do we need to trim the strings for relative paths?
      DO WHILE ( FILE(1:3) .EQ. '../' )
        FILE = FILE(4:)
        I = CHR_LEN( PWD )
        DO WHILE ( PWD(I:I) .NE. '/' )
          PWD(I:I) = ' '
          I = I - 1
        END DO
        PWD(I:I) = ' '
      END DO

*  Put the (trimmed) path and file strings together if not an absolute file
      IF ( FILE(1:1) .NE. '/') THEN
        FILE = PWD(:CHR_LEN(PWD)) // '/' // FILE(:CHR_LEN(FILE))
      END IF

*  Trim the returned FILE of any relative (../) directions
      I = INDEX( FILE, '../' )
      DO WHILE ( I .GT. 0 )
        J = I + 3
        I = I - 2
        DO WHILE ( FILE(I:I) .NE. '/' )
          I = I - 1
        END DO
        FILE = FILE(1:I) // FILE(J:CHR_LEN(FILE))
        I = INDEX( FILE, '../' )
      END DO

*  Extract PATH from the end of FILE
      PATH = FILE
      I = INDEX( PATH, '/' )
      DO WHILE ( I .GT. 0 )
        PATH = PATH(I+1:)
        I = INDEX( PATH, '/' )
      END DO
      I = INDEX( PATH, '.' )
      IF ( I .GT. 0 ) THEN
        PATH = PATH(:I-1)
      END IF
      CALL CHR_UCASE( PATH )

*  Report error if that failed
      IF ( FSTAT .NE. 0 ) THEN
        print*, 'PWD: ', '"'//pwd//'"'
        print*, 'FILE:', '"'//file//'"'
        print*, fstat, status
        call adi_print(args(1), status)
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unable to get file name for FITSfile'/
     :                /' object', STATUS )
      ELSE

*    Create structure and store filenames
        CALL ADI_NEW0( 'STRUC', OARG, STATUS )
        CALL ADI_CPUT0C( OARG, 'File', FILE(:CHR_LEN(FILE)), STATUS )
        CALL ADI_CPUT0C( OARG, 'Path', PATH(:CHR_LEN(PATH)), STATUS )
        CALL ADI_CPUT0I( OARG, 'Nlev', 1, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_FTRACE', STATUS )

      END
