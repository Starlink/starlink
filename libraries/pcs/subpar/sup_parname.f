      SUBROUTINE SUBPAR_PARNAME( NAMECODE, NAME, NAMELEN, STATUS)
*+
*  Name:
*     SUBPAR_PARNAME

*  Purpose:
*     Get the name of a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_PARNAME( NAMECODE, NAME, NAMELEN, STATUS)

*  Description:
*     Get the name of a parameter

*  Arguments:
*     NAMECODE=INTEGER(INPUT)
*        Identifier of the parameter
*     NAME=CHARACTER*(*)(OUTPUT)
*        Name of the parameter
*     NAMELEN=INTEGER(OUTPUT)
*        Length of the parameter name
*     STATUS=INTEGER(UPDATE)
*        SSE status variable

*  Algorithm:
*     Get the name from the global variable

*  Authors:
*     JHF: Jon Fairclough (UKTH::JHF)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-May-1986 (JHF):
*        Original
*     03-MAR-1993 (AJC):
*        Use SAE_PAR not ADAMERRS
*        Add DAT_PAR for SUBPAR_CMN
*     07-SEP-1993 (AJC):
*        Report if error
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! ADAM Symbolic Constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_ERR'

*  Arguments Given:
      INTEGER NAMECODE

*  Arguments Returned:
      CHARACTER*(*) NAME
      INTEGER NAMELEN

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*.

      IF (STATUS .NE. SAI__OK) RETURN
*
*    Begin
*
      IF (NAMECODE .GE. 1 .AND. NAMECODE .LE. SUBPAR__MAXPAR) THEN
         NAME = PARNAMES(NAMECODE)
         NAMELEN = PARLEN(NAMECODE)
      ELSE
         STATUS = SUBPAR__NOPAR
         CALL EMS_REP('SUP_PARNAME1',
     :   'SUBPAR_PARNAME: NAMECODE out of range', STATUS )
      ENDIF
*
*    End
*
      END
