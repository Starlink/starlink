      SUBROUTINE GNS_GTN ( NAME, LNAME, STATUS )

*+
*  Name:
*     GNS_GTN

*  Purpose:
*     Get terminal name

*  Invocation:
*     CALL GNS_GTN( NAME, LNAME, STATUS )

*  Description:
*     The physical device name of the terminal attached to the current
*     process (or its parents) is returned. If there is no terminal
*     available (for example in batch, network or detached processes)
*     the name is set to blanks and the length set to zero.
*
*     If the name is longer than the supplied character variable the
*     name is truncated but the length returned is the length of the
*     actual name.

*  Arguments:
*     NAME = CHARACTER*(GNS__SZTER) (Returned)
*        The device name of the terminal attached to the process
*     LNAME = INTEGER (Returned)
*        The number of characters in the terminal name
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Authors:
*     DLT: D. L. Terrett (STARLINK)
*     NE: Nick Eaton (Durham University)

*  History:
*     18-APR-1990 (DLT):
*        Original version.
*      1-SEP-1992 (NE):
*        Updated prologue.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'SAE_PAR'
      INCLUDE 'GNS_ERR'

*  Arguments Returned:
      CHARACTER*(*) NAME
      INTEGER LNAME

*  Status:
      INTEGER STATUS
*.

      LNAME = 0
      IF (STATUS.EQ.SAI__OK) THEN
         CALL PSX_TTYNAME( 0, NAME, STATUS)
         IF (STATUS.EQ.SAI__OK) THEN
             LNAME = INDEX( NAME, ' ') - 1
         ENDIF
      ENDIF

      END
