      SUBROUTINE IRA1_SFLD( FIELDS, SIGN, FTEXT, FLEN, NC, CONTXT,
     :                      PFLAGS, STATUS )
*+
*  Name:
*     IRA1_SFLD

*  Purpose:
*     suppress the printing of redundant leading fields.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_SFLD( FIELDS, SIGN, FTEXT, FLEN, NC, CONTXT, PFLAGS,
*                     STATUS )

*  Description:
*     This routine compares the fields to be displayed for the current
*     longitude or latitude value, with those displayed for the previous
*     value, and suppresses the display of any leading fields which have
*     not changed, by setting the corresponding elements of PFLAGS to
*     false. No fields are suppressed if any of the following cases
*     apply:
*
*     1) The input CONTXT value is blank.
*
*     2) The input CONTXT value is equal to KEEP.
*
*     3) The axis index has changed since the last value was displayed.
*     The axis index (A or B) is stored as the first character of
*     CONTXT.
*
*     4) The sign has changed.
*
*     On exit CONTXT always holds information describing the current
*     value being displayed, unless CONTXT has the valeu KEEP on entry,
*     in which case CONTXT is left unchanged.
*
*     Values should always be displayed in such an order that the
*     absolute value of the longitude or latitude increases (i.e. start
*     at zero (saving the returned context) and then work upwards. Then
*     go back to zero, reinstate the saved context and work downwards.

*  Arguments:
*     FIELDS( 4 ) = INTEGER (Given)
*        The four integer values of the displayed fields, hours, minutes
*        seconds and fraction of a second, or degrees, arc-minutes,
*        arc-seconds and fraction of an arc-second.
*     SIGN = CHARACTER * ( * ) (Given)
*        The sign of the coordinate value.
*     FTEXT( 4 ) = CHARACTER * ( * ) (Given)
*        The text string corresponding to each integer field value.
*     FLEN( 4 ) = INTEGER (Given)
*        The used no. of characters in each element of FTEXT.
*     NC = INTEGER (Given)
*        The axis index, 1 for longitude (stored as "A" in CONTXT), or 2
*        for latitude (stored as "B" in CONTXT).
*     CONTXT = CHARACTER * ( * ) (Given and Returned)
*        On entry, holds information about the previous value displayed,
*        on exit holds information about the current value being
*        displayed. If equal to "KEEP" on entry then no fields are
*        suppressed and CONTXT is returned unchanged.
*     PFLAGS( 5 ) = LOGICAL (Given and Returned)
*        Flags indicating if each field should be printed or not.
*        Elements 1 to 4 correspond to fields 1 to 4, and element 5
*        corresponds to the sign character. Elements are set false to
*        suppress the printing of the corresponding field, and are left
*        unchanged otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-MAR-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_ERR'          ! IRA error values.

*  Arguments Given:
      INTEGER FIELDS( 4 )
      CHARACTER SIGN*(*)
      CHARACTER FTEXT( 4 )*(*)
      INTEGER FLEN( 4 )
      INTEGER NC

*  Arguments Given and Returned:
      CHARACTER CONTXT*(*)
      LOGICAL PFLAGS( 5 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SPACE1             ! Index within CONTXT of space
                                 ! following the first field.
      INTEGER SPACE2             ! Index within CONTXT of space
                                 ! following the second field.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the user has specified that all fields should be kept, do not
*  remove any redundant fields.
      IF( CONTXT( 1 : 4 ) .NE. 'KEEP' ) THEN

*  Otherwise, find the positions at which the spaces following the
*  first and second fields should be found if neither of the fields has
*  changed.
         SPACE1 = 3 + FLEN( 1 )
         SPACE2 = SPACE1 + FLEN( 2 ) + 1

*  If the context string is too short, report an error.
         IF( LEN( CONTXT ) .LT. SPACE2 ) THEN
            STATUS = IRA__TOOSH
            CALL MSG_SETI( 'L', LEN( CONTXT ) )
            CALL MSG_SETI( 'S', SPACE2 )
            CALL ERR_REP( 'IRA1_SFLD_ERR1',
     :'IRA1_SFLD: Declared length of argument CONTXT (^L) is too '//
     : 'short. It should be at least ^S.', STATUS )
            GO TO 999
         END IF

*  If the supplied coordinate value is of the same type (longitude or
*  latitude) as the value stored in CONTXT, suppress the printing of
*  leading fields which are the same as the corresponding fields in
*  CONTXT.
         IF( ( CONTXT( 1 : 1 ) .EQ. 'A' .AND. NC .EQ. 1 ) .OR.
     :       ( CONTXT( 1 : 1 ) .EQ. 'B' .AND. NC .EQ. 2 ) ) THEN

*  If the sign has changed, all leading fields should be printed (
*  unless the leading field is zero ..
            IF( SIGN .EQ. CONTXT( 2 : 2 ) .OR. FIELDS( 1 ) .EQ. 0) THEN

*  If the first field has not changed, suppress the printing of the first
*  field and the sign.
               IF( CONTXT( 3 : SPACE1 ) .EQ.
     :             FTEXT( 1 )( : FLEN( 1 ) )//' ' ) THEN
                  PFLAGS( 1 ) = .FALSE.
                  PFLAGS( 5 ) = .FALSE.

*  If the second field has not changed, suppress the printing of it.
                  IF( CONTXT( SPACE1 + 1 : SPACE2 ) .EQ.
     :                FTEXT( 2 )( : FLEN( 2 ) )//' ' ) THEN
                     PFLAGS( 2 ) = .FALSE.

*  The third and fourth fields are always printed.

                  END IF

               END IF

            END IF

         END IF

*  Save the current context.
         IF( NC .EQ. 1 ) THEN
            CONTXT( 1 : 1 ) = 'A'
         ELSE
            CONTXT( 1 : 1 ) = 'B'
         END IF

         CONTXT( 2 : 2 ) = SIGN
         CONTXT( 3 : SPACE1 ) = FTEXT( 1 )( : FLEN( 1 ) )//' '
         CONTXT( SPACE1 + 1 : ) = FTEXT( 2 )( : FLEN( 2 ) )//' '

      END IF

*  Don't print any "+" signs.
      IF( SIGN .EQ. '+' ) PFLAGS( 5 ) = .FALSE.

 999  CONTINUE

      END
