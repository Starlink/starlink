      SUBROUTINE POL1_SETFT( NCARD, FITS, CMPNAM, FTNAM, LOC, ICARD,
     :                       COMMNT, NEW, STATUS )
*+
*  Name:
*     POL1_SETFT

*  Purpose:
*     Store a named component of an HDS object as a FITS card.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SETFT( NCARD, FITS, CMPNAM, FTNAM, LOC, ICARD, COMMNT,
*                      NEW, STATUS )

*  Description:
*     This routine stores the value of a named component of the
*     HDS object given by LOC, as a FITS header card using the
*     keyword name given by FTNAM, in an array of character strings.
*     The routine returns without action if the component is not of
*     an HDS primitive type, or if it does not exist.
*
*     If the keyword already exists in the FITS array, then its value is
*     replaced. Otherwise, the new card is stored at index ICARD.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of cards to be searched in the FITS array.
*     FITS( * ) = CHARACTER * ( * ) (Given)
*        The array holding the FITS cards.
*     CMPNAM = CHARACTER * ( * ) (Given)
*        The name of the HDS component to be stored.
*     FTNAM = CHARACTER * ( * ) (Given)
*        The name of the FITS keyword to be used.
*     LOC = CHARACTER * ( * ) (Returned)
*        An HDS locator to the object containing the component to be
*        stored.
*     ICARD = INTEGER (Given)
*        The index within FITS at which to store the named component, if
*        an existing card for the keyword cannot be found.
*     COMMNT = CHARACTER * ( * ) (Given)
*        The comment to include in the FITS header card.
*     NEW = LOGICAL (Returned)
*        Was no existing card found for the specified keyword?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-DEC-1997 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants

*  Arguments Given:
      INTEGER NCARD
      CHARACTER FITS( * )*(*)
      CHARACTER CMPNAM*(*)
      CHARACTER FTNAM*(*)
      CHARACTER LOC*(*)
      INTEGER ICARD
      CHARACTER COMMNT*(*)

*  Arguments Returned:
      LOGICAL NEW

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      CHARACTER COM*80           ! Existing card's comment
      CHARACTER CVAL*80          ! Character component value
      CHARACTER TYPE*(DAT__SZTYP)! The comnponent type
      DOUBLE PRECISION DVAL      ! Double precision component value
      INTEGER IVAL               ! Integer component value
      INTEGER JCARD              ! Index at which to store the card
      LOGICAL LVAL               ! Logical component value
      LOGICAL THERE              ! Does it exist?
      REAL RVAL                  ! Real component value
      INTEGER CLEN
*.

      NEW = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the named component exists in the HDS object. Do nothing
*  if it does not exist.
      CALL DAT_THERE( LOC, CMPNAM, THERE, STATUS )
      IF( THERE ) THEN

*  Get the component's type.
         CALL CMP_TYPE( LOC, CMPNAM, TYPE, STATUS )

*  See if the keyword already exists in the FITS array.
         CALL FTS1_GKEYC( NCARD, FITS, 1, FTNAM, 1, THERE,
     :                    CVAL, COM, JCARD, STATUS )

*  If not, create a new card and store it at the supplied index. Otherwise
*  store it at the index of the existing card.
         IF( .NOT. THERE ) THEN
            JCARD = ICARD
            NEW = .TRUE.
         ELSE
            NEW = .FALSE.
         END IF

*  Process each primitive type in turn. Ignore non-primitive values.
         IF( TYPE .EQ. '_REAL' ) THEN

*  Get the component's value.
            CALL CMP_GET0R( LOC, CMPNAM, RVAL, STATUS )

*  Format it as a FITS card using the supplied FITS keyword name.
            CALL FTS1_WKEYR( FTNAM, RVAL, '/', COMMNT, FITS( JCARD ),
     :                       STATUS )

*  Do the same for _DOUBLE values.
         ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
            CALL CMP_GET0D( LOC, CMPNAM, DVAL, STATUS )
            CALL FTS1_WKEYD( FTNAM, DVAL, '/', COMMNT, FITS( JCARD ),
     :                       STATUS )

*  Access all integer types as _INTEGER.
         ELSE IF( TYPE .EQ. '_INTEGER' .OR.
     :            TYPE .EQ. '_WORD' .OR.
     :            TYPE .EQ. '_UWORD' .OR.
     :            TYPE .EQ. '_BYTE' .OR.
     :            TYPE .EQ. '_UBYTE' ) THEN
            CALL CMP_GET0I( LOC, CMPNAM, IVAL, STATUS )
            CALL FTS1_WKEYI( FTNAM, IVAL, '/', COMMNT, FITS( JCARD ),
     :                       STATUS )

*  Now do logical values.
         ELSE IF( TYPE .EQ. '_LOGICAL' ) THEN
            CALL CMP_GET0L( LOC, CMPNAM, LVAL, STATUS )
            CALL FTS1_WKEYL( FTNAM, LVAL, '/', COMMNT, FITS( JCARD ),
     :                       STATUS )

*  Now do character values. Strip trailing spaces.
         ELSE IF( TYPE( : 5 ) .EQ. '_CHAR' ) THEN
            CALL CMP_GET0C( LOC, CMPNAM, CVAL, STATUS )
            CLEN = MAX( 1, CHR_LEN( CVAL ) )
            CALL FTS1_WKEYC( FTNAM, CVAL( : CLEN ), '/', COMMNT,
     :                      .FALSE., FITS( JCARD ), STATUS )

         END IF

      END IF

      END
