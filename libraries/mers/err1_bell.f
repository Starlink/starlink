      SUBROUTINE ERR1_BELL( STATUS )
*+
*  Name:
*     ERR1_BELL

*  Purpose:
*     Deliver an ASCII BEL character.

*  Language:
*    Starlink Fortran 77

*  Invocation:
*     CALL ERR1_BELL( STATUS )

*  Description:
*     A bell character is delivered to the user. If the user interface 
*     in use supports this character, this will ring a bell on the 
*     terminal.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The global status.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1993 (PCTR):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
 
*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER ASCBEL                    ! ASCII BEL code
      PARAMETER ( ASCBEL = 7 )

*  Local Variables:
      CHARACTER BELCHR * 1              ! The bell character

      INTEGER ISTAT                     ! Local status

*.

*  Initialize the local status.
      ISTAT = SAI__OK

*  Use SUBPAR_WRERR to deliver the bell character.
      BELCHR = CHAR( ASCBEL )
      CALL ERR1_PRERR( BELCHR, ISTAT )

*  Check the local status and return it on error.
      IF ( ISTAT .NE. SAI__OK ) STATUS = ISTAT

      END
