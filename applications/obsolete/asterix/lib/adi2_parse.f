      SUBROUTINE ADI2_PARSE( FSPEC, LFILEC, HDU, KEYWRD, STATUS )
*+
*  Name:
*     ADI2_PARSE

*  Purpose:
*     Parse a FITS file specification.

*  Language:
*     Fortran

*  Invocation:
*     CALL ADI2_PARSE( FSPEC, LFILEC, HDU, KEYWRD, STATUS )

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
*     KEYWRD = CHAR (Returned)
*        File access mode
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
*     {routine_references}...

*  Keywords:
*     {routine_keywords}...

*  Copyright:
*     {routine_copyright}

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
      INTEGER			HDU
      CHARACTER*(*)		KEYWRD

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      INTEGER                   CIP			! Position of ^
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
      HDU = 0
      KEYWRD = ' '

*    HDU delimiter present?
      ICP = INDEX( FSPEC, '[' )
      IF ( ICP .EQ. 0 ) THEN
        ICP = INDEX( FSPEC, '+' )
      END IF

*    Keyword delimiter present?
      CIP = INDEX( FSPEC, '^' )

*    Delimiter present, but in illegal position
      IF ( (ICP .EQ. 1) .OR. (ICP.GE.FLEN) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Illegal FITS filename specification',
     :                STATUS )

*    Does filename contain HDU specification?
      ELSE IF ( ICP .GT. 1 ) THEN

*      Adjust last filename character if HDU spec found
        LFILEC = ICP - 1

*      HDU bracketed syntax?
        IF ( FSPEC(ICP:ICP) .EQ. '[' ) THEN

*        Look for closing bracket
          NCP = INDEX(FSPEC(ICP:),']')

*        Not found?
          IF ( NCP .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Missing "]" in HDU extension '/
     :                    /'specification', STATUS )

          ELSE IF ( NCP .EQ. 2 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Empty HDU extension '/
     :                    /'specification', STATUS )

          ELSE
            NCP = ICP + NCP - 1
            CALL CHR_CTOI( FSPEC(ICP+1:NCP-1), HDU, STATUS )
            IF ( (STATUS .NE. SAI__OK) .OR. (HDU.LT.1) ) THEN
              CALL MSG_SETC( 'NUM', FSPEC(ICP+1:NCP-1) )
              CALL ERR_REP( ' ', 'Illegal HDU number /^NUM/', STATUS )
            END IF

          END IF

*      Incremental syntax
        ELSE

*        The HDU number in incremental syntax is delimited to the right
*        by either the end of the string, or the beginning of a keyword
*        name.
          NCP = CIP
          IF ( NCP .EQ. 0 ) THEN
            NCP = FLEN
          ELSE
            NCP = NCP - 1
          END IF
          CALL CHR_CTOI( FSPEC(ICP+1:NCP), HDU, STATUS )
          IF ( (STATUS .NE. SAI__OK) .OR. (HDU.LT.1) ) THEN
            CALL MSG_SETC( 'NUM', FSPEC(ICP+1:NCP) )
            CALL ERR_REP( ' ', 'Illegal HDU number /^NUM/', STATUS )
          END IF

        END IF

      END IF

*    Look for keyword specification
      IF ( (STATUS .EQ. SAI__OK) .AND. (CIP.GT.0) ) THEN
        KEYWRD = FSPEC(CIP+1:)
        LFILEC = MIN(LFILEC,CIP-1)
      END IF

      END
