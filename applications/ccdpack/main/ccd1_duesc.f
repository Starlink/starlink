      SUBROUTINE CCD1_DUESC( STRING, STATUS )
*+
*  Name:
*     CCD1_DUESC

*  Purpose:
*     To duplicate any MSG system escape characters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_DUESC( STRING, STATUS )

*  Description:
*     This routine loops through a given string looking for MSG system
*     escape characters, if it finds any then they are duplicated. This
*     allows a string which has been MSG_LOADed to be passed back
*     through the MSG_OUT routine with out secondary token replacement.
*     It can also be used on loaded NDF_MSG strings etc. which have
*     device characters like $ etc.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The character buffer which may contain escape sequences in need
*        of duplication before passing back through the message system.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-JUN-1991 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Size of MSG system buffers

*  Arguments Given:
      CHARACTER * ( * ) STRING

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NUMESC             ! The number of escape sequences.
      PARAMETER ( NUMESC = 3 )

*  Local Variables:
      INTEGER STRLEN             ! Length of string
      INTEGER IAT                ! Position of escape sequence within
                                 ! message
      INTEGER I                  ! Loop counter
      INTEGER START              ! Present start of substring to look
                                 ! for escape characters in.
      CHARACTER ESCAPE * ( NUMESC ) ! MSG escape characters
      CHARACTER NEWBUF * ( MSG__SZMSG ) ! Work space buffer.

*  Local Data:
      DATA ESCAPE / '^%$'/

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      STRLEN = LEN( STRING )

*  Try to trap any escape sequences present in the line of data.
      DO 1 I = 1, NUMESC
         START = 1
 2       CONTINUE         ! Do while
            IAT = INDEX( STRING( START : STRLEN ), ESCAPE( I : I ) )
            IF ( IAT .NE. 0 ) THEN

*  Correct the offset to true string position
               IAT = IAT + START - 1

*  Have an escape sequence. Replace with two.
               CALL CCD1_INSER( ESCAPE( I : I ), STRING( :STRLEN ),
     :                          IAT, NEWBUF, STATUS )

*  Look for the next repeat of this escape sequence
               START = IAT + 2
               STRLEN = STRLEN + 1
               STRING = NEWBUF
               IF ( IAT .LT. STRLEN ) GO TO 2
            END IF
 1    CONTINUE

      END
* $Id$
