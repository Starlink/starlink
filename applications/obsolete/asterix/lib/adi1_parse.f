      SUBROUTINE ADI1_PARSE( FSPEC, LFILEC, FSUBC, LSUBC, STATUS )
*+
*  Name:
*     ADI1_PARSE

*  Purpose:
*     Parse an HDS file specification.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_PARSE( FSPEC, LFILEC, FSUBC, LSUBC, STATUS )

*  Description:
*     Parse an HDS object specification. This consists of a filename with
*     the filetype ".sdf" omitted, followed by optional substructure,
*     followed optionally by %hds.

*  Arguments:
*     FSPEC = CHAR (Given)
*        Name of the object on which FITS access to be attempted
*     LFILEC = INTEGER (Returned)
*        Index of last character in filemname proper
*     FSUBC = INTEGER (Returned)
*        First character of sub-structure if present. Zero otherwise
*     LSUBC = INTEGER (Returned)
*        Last character of sub-structure if present. Zero otherwise
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

*  Arguments Returned:
      INTEGER			LFILEC
      INTEGER			FSUBC,LSUBC

*  Status:
      INTEGER 			STATUS             	! Global status

*  External references:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      INTEGER			FLEN			! Length of FSPEC
      INTEGER                   ICP			! Character index
      INTEGER                   NCP			! Character index
*.

*    Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Length of FSPEC
      FLEN = CHR_LEN( FSPEC )

*    Look for % delimiter, so that trailing representation can be ignored
      ICP = FLEN
      DO WHILE ( (ICP.GT.0) .AND. (FSPEC(ICP:ICP).NE.'%') )
        ICP = ICP - 1
      END DO
      IF ( ICP .NE. 0 ) FLEN = ICP - 1

*    Default values
      LFILEC = FLEN
      LSUBC = 0
      FSUBC = 0

*    Find first character after any slash present
      ICP = LFILEC
      DO WHILE ( (ICP.GT.0) .AND. (FSPEC(ICP:ICP).NE.'/') )
        ICP = ICP - 1
      END DO
      ICP = ICP + 1

*    Position of period after this point? If present then this denotes
*    sub-structure start.
      NCP = INDEX( FSPEC(ICP:LFILEC), '.' )
      IF ( NCP .GT. 0 ) THEN
        NCP = NCP + ICP - 1
        FSUBC = NCP + 1
        LSUBC = LFILEC
        LFILEC = NCP - 1
      END IF

      END
