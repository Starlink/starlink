      SUBROUTINE IRC1_CHKUN( INDF, STATUS )
*+
*  Name:
*     IRC1_CHKUN

*  Purpose:
*     Check that the NDF component UNITS has a valid value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_CHKUN( INDF, STATUS )

*  Description:
*     The value of the UNITS component is obtained from the given NDF,
*     and a list of valid values is obtained using IRC_IUNIT. The
*     value obtained from the NDF is then compared with each item in
*     the list. If an exact case-insensitive match is not found
*     an error report is made.

*  Arguments:
*     IN = INTEGER (Given)
*        The NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JAN-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC errors

*  Arguments Given:
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CHR_LEN            ! CHR_ function giving used length.
      LOGICAL CHR_SIMLR          ! CHR_ string comparison function.
      INTEGER END                ! End position of a name.
      INTEGER FIRST              ! Position of first used character in
                                 ! NDFUN.
      INTEGER LAST               ! Position of last used character in
                                 ! NDFUN.
      LOGICAL MORE               ! True if more SCS names remain.
      CHARACTER NDFUN*(IRC__SZUNI)! UNITS value from the NDF.
      INTEGER NMATCH             ! No. of matches found between IN and
                                 ! LIST.
      INTEGER START              ! Start position of a name.
      CHARACTER VALIST*(IRC__SZULS)! List of legal UNITS values.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the value from the NDF.
      CALL NDF_CGET( INDF, 'UNITS', NDFUN, STATUS )

*  If a blank value was obtained, give an error report.
      IF( STATUS .EQ. SAI__OK .AND. NDFUN .EQ. ' ' ) THEN
         STATUS = IRC__NOUNI
         CALL ERR_REP( 'IRC1_CHKUN_ERR1',
     :        'IRC1_CHKUN: A blank value found for NDF component UNITS',
     :                 STATUS )
      END IF

      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Find the start and end of the used portion.
      CALL CHR_FANDL( NDFUN, FIRST, LAST )

*  Get a list of legal values.
      CALL IRC_IUNIT( VALIST, STATUS )

*  Loop round each value in the list.
      MORE = .TRUE.
      START = 1
      NMATCH = 0

      DO WHILE( MORE )

*  Get the next value from the list.
         END=INDEX( VALIST(START:), ',' ) + START - 2
         IF( END .EQ. START - 2 ) THEN
            END = CHR_LEN( VALIST )
            MORE = .FALSE.
         END IF

*  See if the value obtained from the NDF matches this string, ignoring
*  differences in case and leading blanks.
         IF( CHR_SIMLR( VALIST(START:END), NDFUN(FIRST:LAST) ) ) THEN
            NMATCH = NMATCH + 1
         END IF

*  Set the start of the next value in the list
         START = END + 2

      END DO

*  If no matches were found, give an error.
      IF( NMATCH .EQ. 0 ) THEN
         STATUS = IRC__BADUN
         CALL MSG_SETC( 'UN', NDFUN )
         CALL ERR_REP( 'IRC1_CHKUN_ERR2',
     :   'IRC1_CHKUN: Illegal value found for NDF component UNITS: ^UN',
     :                 STATUS )
      END IF

 999  CONTINUE

      END
