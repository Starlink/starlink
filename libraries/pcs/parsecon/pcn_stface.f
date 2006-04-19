      SUBROUTINE PARSECON_STFACE ( STATUS )
*+
*  Name:
*     PARSECON_STFACE

*  Purpose:
*     Action on INTERFACE field.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_STFACE ( STATUS )

*  Description:
*     Performs the ENDINTERFACE action if the previous ENDINTERFACE is
*     missing

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     Looks at ACNAME. If it is set, the ENDINTERFACE must have been missing
*     so a message is reported and the ENDINTERFACE action routine is called.
*     This checks for correct POSITION numbering.

*  Authors:
*     A J Chipperfield (RLVAD::AJC)
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25.02.1992:  Original (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PARSECON_ERR'


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'
      INCLUDE 'PARSECON3_CMN'

*    Local Variables
      INTEGER ISTAT               ! Local status

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( ACNAME .NE. ' ' ) THEN
*   The ENDINTERFACE must have been missing
         ISTAT = PARSE__MISEND
         CALL EMS_REP ( 'PCN_STFACE1',
     :   'PARSECON: Missing ENDINTERFACE', ISTAT )

         CALL PARSECON_FACEND ( STATUS )
*   Ensure that some error is reported
         IF ( STATUS .EQ. SAI__OK ) STATUS = PARSE__MISEND

      ENDIF

      END
