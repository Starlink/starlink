      SUBROUTINE PON_RMLOPT( OPTNAM, STATUS )
*+
*  Name:
*     PON_RMLOPT

*  Purpose:
*     Remove the first L from the PGPLOT option string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_RMLOPT( OPTNAM, STATUS )

*  Description:
*     Remove the first L, signifying a logarithmic options, from the
*     PGPLOT axis option string.

*  Arguments:
*     OPTNAM = CHARACTER * ( * ) (Given)
*        Name of option parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPERL P.W. Draper (Starlink - Durham University)
*     {enter_new_authors_here}

*  History:
*     22-JUN-1990 (JBVAD::PAH):
*        Original version.
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     17-FEB-1995 (PDRAPER):
*        Corrected loop that removes 'L' to also copy the rest of
*        the string (Loop index wasn't used).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) OPTNAM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Index
      INTEGER IVAL               ! Loop index

      CHARACTER * ( 30 ) OPTVAL  ! Value of the axis options

*.

*  Check inherited global status.
      IF ( STATUS.NE.SAI__OK ) RETURN

      CALL PAR_GET0C( OPTNAM, OPTVAL, STATUS )
      CALL CHR_UCASE( OPTVAL )
      I = INDEX( OPTVAL, 'L' )

*  If there is an 'L' at the start of the string, remove it.
      IF ( I.EQ.1 ) THEN
         DO 10 IVAL = 1, 29
            OPTVAL( IVAL:IVAL ) = OPTVAL( IVAL+1:IVAL+1 )
 10      CONTINUE

         OPTVAL( 30 : ) = ' '

*     Put the new parameter value back into the parameter system.
         CALL PAR_PUT0C( OPTNAM, OPTVAL, STATUS )
      END IF

      END
* $Id$
