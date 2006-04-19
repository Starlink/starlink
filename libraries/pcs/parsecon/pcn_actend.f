      SUBROUTINE PARSECON_ACTEND ( STATUS )
*+
*  Name:
*     PARSECON_ACTEND

*  Purpose:
*     on ENDACTION check for ACTKEY clashes.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_ACTEND ( STATUS )

*  Description:
*     Check the list of action keywords for a clash
*     and remove the action name from the error report common block.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     The list of action keywords is searched sequentially
*     and set ACNAME to blank.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26.05.1987:  Original (REVAD::BDK)
*     16.08.1990:  Reset ACNAME for error reports (RLVAD::AJC)
*     24.02.1992:  Report errors (RLVAD::AJC)
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


*  Local Variables:
      INTEGER NAMECODE                  ! counter for searching

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      DO NAMECODE = 1, ACTPTR-1

         IF ( ACTKEY(ACTPTR) .EQ. ACTKEY(NAMECODE) ) THEN
           STATUS = PARSE__OLDACTKEY
           CALL EMS_REP( 'PCN_ACTEND1',
     :     'PARSECON: Action Keyword multiply defined', STATUS )
         ENDIF

      ENDDO

      ACNAME = ' '

      END
