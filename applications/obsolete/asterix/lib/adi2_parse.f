      SUBROUTINE ADI2_PARSE( FSPEC, LFILEC, HDU, FPATH, FPLEN, STATUS )
*+
*  Name:
*     ADI2_PARSE

*  Purpose:
*     Parse a FITS file specification.

*  Language:
*     Fortran

*  Invocation:
*     CALL ADI2_PARSE( FSPEC, LFILEC, HDU, FPATH, STATUS )

*  Description:
*     Attempts to open the named file object as an HDS file. If successful
*     the routine stores the logical unit and HDU number on the property
*     list of the ID object.

*  Arguments:
*     FSPEC = CHAR (Given)
*        Name of the object on which FITS access to be attempted
*     LFILEC = INTEGER (Returned)
*        Index of last character in filemname proper
*     HDU = INTEGER (Returned)
*        HDU number, zero if none
*     FPATH = CHARACTER*(*) (Returned)
*        Sub-file path
*     FPLEN = INTEGER
*        Length of FPATH, or zero if none (Returned)
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

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
      INTEGER			LFILEC, HDU, FPLEN
      CHARACTER*(*)		FPATH

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN
      EXTERNAL			CHR_ISDIG
        LOGICAL			CHR_ISDIG

*  Local Variables:
      INTEGER			FLEN			! Length of FSPEC
      INTEGER                   ICP			! Character index
      INTEGER                   NCP			! Character index
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Length of FSPEC
      FLEN = CHR_LEN( FSPEC )

*  Look for % delimiter, so that trailing representation can be ignored
      ICP = FLEN
      DO WHILE ( (ICP.GT.0) .AND. (FSPEC(ICP:ICP).NE.'%') )
        ICP = ICP - 1
      END DO
      IF ( ICP .NE. 0 ) FLEN = ICP - 1

*  Default values
      LFILEC = FLEN
      HDU = 0
      FPLEN = 0

*  HDU delimiter present?
      ICP = INDEX( FSPEC, '[' )
      IF ( ICP .EQ. 0 ) THEN
        ICP = INDEX( FSPEC, '+' )
        IF ( ICP .GT. 0 ) THEN

*      Skip over digits of HDU number
          ICP = ICP + 1
          NCP = ICP
          DO WHILE ( CHR_ISDIG( FSPEC(NCP:NCP) ) )
            NCP = NCP + 1
          END DO
          NCP = NCP - 1

*      If no digits signal error
          IF ( NCP .LT. ICP ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Missing HDU number in extension '/
     :                    /'specification', STATUS )
          END IF

        END IF

      ELSE
        ICP = ICP + 1
        NCP = INDEX( FSPEC, ']' )
        IF ( NCP .EQ. 0 ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Missing "]" in HDU extension '/
     :                  /'specification', STATUS )

        ELSE IF ( NCP .LE. ICP ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Misplaced "]" in HDU extension '/
     :                  /'specification', STATUS )

        ELSE
          NCP = NCP - 1

        END IF

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Sub-HDU structure may only appear after an HDU specification. NCP marks
*  the last character of the HDU specification
      IF ( ICP .GT. 0 ) THEN

*    Last character in the filename proper
        LFILEC = ICP - 2

*    Extract HDU number
        CALL CHR_CTOI( FSPEC(ICP:NCP), HDU, STATUS )
        IF ( (STATUS .NE. SAI__OK) .OR. (HDU.LT.1) ) THEN
          CALL MSG_SETC( 'NUM', FSPEC(ICP+1:NCP) )
          CALL ERR_REP( ' ', 'Illegal HDU number /^NUM/', STATUS )
        END IF

*    Encode the HDU number into FPATH
        CALL CHR_ITOC( HDU, FPATH(2:), FPLEN )
        FPATH(1:1) = '+'
        FPLEN = FPLEN + 1

*    Remaining text after end of HDU spec?
        IF ( FSPEC(NCP+1:NCP+1) .EQ. ']' ) THEN
          FPATH(FPLEN+1:) = FSPEC(NCP+2:FLEN)
          FPLEN = FPLEN + FLEN - NCP - 1
        ELSE
          FPATH(FPLEN+1:) = FSPEC(NCP+1:FLEN)
          FPLEN = FPLEN + FLEN - NCP
        END IF
        CALL CHR_UCASE( FPATH )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Look for sub-file specification
 99   CONTINUE

      END
