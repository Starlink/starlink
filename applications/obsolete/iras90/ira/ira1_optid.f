      SUBROUTINE IRA1_OPTID( ITEM, INDX, STATUS )
*+
*  Name:
*     IRA1_OPTID

*  Purpose:
*     Get the index of an IRA graphics option.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_OPTID( ITEM, INDX, STATUS )

*  Description:
*     This routine returns the index into the common array ACM_DROPT
*     at which a specified graphics option is stored.

*  Arguments:
*     ITEM = CHARACTER * ( * ) Given)
*        The name of the option. An unambiguous abbreviation may be
*        supplied. Case is ignored.
*     INDX = INTEGER (Returned)
*        The index into ACM_DROPT at which the options value is stored.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-FEB-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Arguments Given:
      CHARACTER ITEM*(*)

*  Arguments Returned:
      INTEGER INDX

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string.

*  Local Constants:
      INTEGER MAXLEN             ! Max. length of an options name.
      PARAMETER ( MAXLEN = 11 )

*  Local Variables:
      INTEGER I                  ! Loop count
      INTEGER LITEM              ! No. of characters in UCITEM.
      INTEGER NMATCH             ! No. of matches been the supplied
                                 ! string and the available options.
      CHARACTER OPT( IRA__NOPT )*( MAXLEN )! Option names.
      CHARACTER UCITEM*( MAXLEN ) ! Upper case copy of ITEM.

*  Local Data:
      DATA OPT / 'TEXT_SIZE',
     :           'COORD_SIZE',
     :           'TOLERANCE',
     :           'LINES',
     :           'LONG_GAP',
     :           'LAT_GAP',
     :           'PEN1',
     :           'PEN2',
     :           'LAT_ACC',
     :           'LONG_ACC',
     :           'PEN3',
     :           'PEN4' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Produce an upper case copy of the supplied string, and remove leading
*  blanks.
      UCITEM = ITEM
      CALL CHR_UCASE( UCITEM )
      CALL CHR_LDBLK( UCITEM )

*  Get the no. of characters in the supplied string.
      LITEM = CHR_LEN( UCITEM )

*  Initialise the no. of matches found between the supplied string and
*  the list of recognised options.
      NMATCH = 0

*  Identify the supplied string.
      DO I = 1, IRA__NOPT

         IF( INDEX( OPT( I ), UCITEM( : LITEM ) ) .EQ. 1 ) THEN
            INDX = I
            NMATCH = NMATCH + 1
         END IF

      END DO

*  If no match was found, report an error.
      IF( NMATCH .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'S', UCITEM )
         CALL ERR_REP( 'IRA1_OPTID_ERR1',
     :                 'IRA1_OPTID: Unrecognised graphics option "^S".',
     :                 STATUS )

*  If more than one match was found, report an error.
      ELSE IF( NMATCH .GT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'S', UCITEM )
         CALL ERR_REP( 'IRA1_OPTID_ERR2',
     :                 'IRA1_OPTID: Ambiguous graphics option "^S".',
     :                 STATUS )

      END IF

      END
